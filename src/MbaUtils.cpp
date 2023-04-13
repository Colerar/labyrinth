#include "labyrinth/MbaUtils.h"

#include "Eigen/Dense"
#include "llvm/IR/IRBuilder.h"

#include <algorithm>
#include <array>

#include "effolkronium/random.hpp"

using llvm::Constant, llvm::ConstantInt, llvm::ConstantArray,
    llvm::ConstantDataArray, llvm::Type, llvm::ArrayType, llvm::StructType,
    llvm::IntegerType, llvm::Function, llvm::FunctionType, llvm::BasicBlock,
    llvm::IRBuilder, llvm::GlobalVariable, llvm::BinaryOperator, llvm::Value,
    llvm::GlobalValue, llvm::Instruction;

using Random = effolkronium::random_thread_local;

namespace labyrinth {

const int8_t truth_table[15][4] = {
    {0, 0, 0, 1}, // x & y
    {0, 0, 1, 0}, // x & ~y
    {0, 0, 1, 1}, // x
    {0, 1, 0, 0}, // ~x & y
    {0, 1, 0, 1}, // y
    {0, 1, 1, 0}, // x ^ y
    {0, 1, 1, 1}, // x | y
    {1, 0, 0, 0}, // ~(x | y)
    {1, 0, 0, 1}, // ~(x ^ y)
    {1, 0, 1, 0}, // ~y
    {1, 0, 1, 1}, // x | ~y
    {1, 1, 0, 0}, // ~x
    {1, 1, 0, 1}, // ~x | y
    {1, 1, 1, 0}, // ~(x & y)
    {1, 1, 1, 1}, // -1
};

std::vector<int64_t> generateLinearMBA(int exprNumber) {
  std::vector<int> expr_selector(exprNumber);
  std::vector<int64_t> coeffs(15);
  while (true) {
    std::fill(coeffs.begin(), coeffs.end(), 0);
    for (auto &item : expr_selector) {
      item = Random::get<int>(0, 14);
    }
    Eigen::MatrixXd A(4, exprNumber);
    Eigen::VectorXd b(4);
    b << 0, 0, 0, 0;
    Eigen::VectorXd X;
    for (int i = 0; i < exprNumber; i++) {
      for (int j = 0; j < 4; j++) {
        A(j, i) = truth_table[expr_selector[i]][j];
      }
    }
    X = A.fullPivLu().kernel().col(0);

    // reject if coefficients contain non-integer or are all zero
    bool reject = false;
    for (int i = 0; i < exprNumber; i++) {
      // NOLINTNEXTLINE
      coeffs[expr_selector[i]] += X[i];
      // NOLINTNEXTLINE
      if (std::abs(X[i] - (int64_t)X[i]) > 1e-5) {
        reject = true;
        break;
      }
    }
    if (reject) {
      continue;
    }
    if (std::any_of(coeffs.begin(), coeffs.end(),
                    [](int64_t i) { return i != 0; })) {
      continue;
    }

    return coeffs;
  }
}

Value *insertLinearMBA(std::vector<int64_t> &params,
                       Instruction *insertBefore) {
  IRBuilder<> ir(insertBefore->getContext());
  ir.SetInsertPoint(insertBefore);

  Value *x, *y;
  if (isa<BinaryOperator>(insertBefore) &&
      insertBefore->getNumOperands() == 2) {
    x = insertBefore->getOperand(0);
    y = insertBefore->getOperand(1);
  } else {
    auto &M = *insertBefore->getModule();
    auto *ty = insertBefore->getOperand(0)->getType();
    auto const_x = ConstantInt::get(ty, Random::get<std::uint64_t>());
    auto const_y = ConstantInt::get(ty, Random::get<uint64_t>());
    auto *x_ptr = new GlobalVariable(M, ty, false, GlobalValue::PrivateLinkage,
                                     const_x, "mba_x");
    auto *y_ptr = new GlobalVariable(M, ty, false, GlobalValue::PrivateLinkage,
                                     const_y, "mba_y");
    x = ir.CreateLoad(ty, x_ptr);
    y = ir.CreateLoad(ty, y_ptr);
  }

  auto x_ty = x->getType();
  Value *mba_expr = ir.CreateAlloca(x_ty);
  ir.CreateStore(ConstantInt::get(x_ty, 0), mba_expr);
  mba_expr = ir.CreateLoad(x_ty, mba_expr);
  Value *bool_expr, *term;
  for (int i = 0; i < 15; i++) {
    if (!params[i])
      continue;
    // x & y
    if (i == 0)
      bool_expr = ir.CreateAnd(x, y);
    // x & ~y
    else if (i == 1)
      bool_expr = ir.CreateAnd(x, ir.CreateNot(y));
    // x
    else if (i == 2)
      bool_expr = x;
    // ~x & y
    else if (i == 3)
      bool_expr = ir.CreateAnd(ir.CreateNot(x), y);
    // y
    else if (i == 4)
      bool_expr = y;
    // x ^ y
    else if (i == 5)
      bool_expr = ir.CreateXor(x, y);
    // x | y
    else if (i == 6)
      bool_expr = ir.CreateOr(x, y);
    // ~(x | y)
    else if (i == 7)
      bool_expr = ir.CreateNot(ir.CreateOr(x, y));
    // ~(x ^ y)
    else if (i == 8)
      bool_expr = ir.CreateNot(ir.CreateXor(x, y));
    // ~y
    else if (i == 9)
      bool_expr = ir.CreateNot(y);
    // x | ~y
    else if (i == 10)
      bool_expr = ir.CreateOr(x, ir.CreateNot(y));
    // ~x
    else if (i == 11)
      bool_expr = ir.CreateNot(x);
    // ~x | y
    else if (i == 12)
      bool_expr = ir.CreateOr(ir.CreateNot(x), y);
    // ~(x & y)
    else if (i == 13)
      bool_expr = ir.CreateNot(ir.CreateAnd(x, y));
    // -1
    else if (i == 14)
      bool_expr = ConstantInt::get(x_ty, -1);
    term = ir.CreateMul(ConstantInt::get(x_ty, params[i]), bool_expr);
    mba_expr = ir.CreateAdd(mba_expr, term);
  }
  return mba_expr;
}

std::tuple<uint64_t, uint64_t, uint64_t> xgcd(uint64_t a, uint64_t b) {
  uint64_t s = 0, old_s = 1;
  uint64_t t = 1, old_t = 0;
  uint64_t r = b, old_r = a;

  while (r != 0) {
    uint64_t q = old_r / r;
    std::swap(r, old_r);
    r -= q * old_r;
    std::swap(s, old_s);
    s -= q * old_s;
    std::swap(t, old_t);
    t -= q * old_t;
  }

  assert(old_r != 0);
  assert(a % old_r == 0);
  assert(b % old_r == 0);
  assert(old_r == old_s * a + old_t * b);

  return std::make_tuple(old_s, old_t, old_r);
}

uint64_t inv(uint64_t a, uint64_t p) {
  auto [x, y, r] = xgcd(a, p);
  // get the inverse element
  return (x % p + p) % p;
}

void generateUnivariatePoly(std::array<uint64_t, 2> &a,
                            std::array<uint64_t, 2> &b, uint32_t bitWidth) {
  assert(bitWidth <= 32);

  uint64_t a0 = Random::get<uint64_t>();
  uint64_t a1 = Random::get<uint64_t>() | 1;

  // Calculate a1_inv
  uint64_t a1_inv = inv(a1, 1LL << bitWidth);

  // Calculate b1
  uint64_t b1 = a1_inv;

  // Calculate b0
  uint64_t b0 = -(b1 * a0);

  a[0] = a0;
  a[1] = a1;
  b[0] = b0;
  b[1] = b1;
}

Value *insertPolynomialMBA(Value *linear_mba_expr, Instruction *insertBefore) {
  Type *op_ty = insertBefore->getOperand(0)->getType();
  uint32_t width = op_ty->getIntegerBitWidth();
  std::array<uint64_t, 2> a{}, b{};
  generateUnivariatePoly(a, b, width);

  IRBuilder<> ir(insertBefore->getContext());
  ir.SetInsertPoint(insertBefore);
  Value *expr;
  expr = ir.CreateMul(ConstantInt::get(op_ty, b[1]), linear_mba_expr);
  expr = ir.CreateAdd(expr, ConstantInt::get(op_ty, b[0]));
  expr = ir.CreateMul(ConstantInt::get(op_ty, a[1]), expr);
  expr = ir.CreateAdd(expr, ConstantInt::get(op_ty, a[0]));
  return expr;
}
} // namespace labyrinth
