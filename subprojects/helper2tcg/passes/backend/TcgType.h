#pragma once

#include "llvm/ADT/Twine.h"

#include <stdint.h>
#include <string>
#include <assert.h>

using llvm::Twine;

//
// Currently using N = 7 bits to represent TcgVKind
//
// If we use a number in base B = 2^N-1 to represent our
// ArgKindHash where each digit corresponds to the arguments
// allowed at that index.  Then
//
//    B*B^0 + B*B^1 + B*B^2 + ... + B*B^M
//
// fits within 64-bit when <= 8, meaning we can represent
// at most 8 arguments, which is more than enough for all
// tcg_gen_* functions.
// TODO(anjo): Update comment
//
using ArgKindMask = uint64_t;

enum TcgVKind : uint8_t {
  IrValue       = 1 << 0,
  IrConst       = 1 << 1,
  IrEnv         = 1 << 2,
  IrImmediate   = 1 << 3,
  IrPtr         = 1 << 4,
  IrPtrToOffset = 1 << 5,
  IrLabel       = 1 << 6,
};

enum TcgSize : uint8_t {
  Tcg32 = 32,
  Tcg64 = 64,
};

constexpr uint64_t ArgKindBits = 7;

struct ConstantExpression {
  std::string Expression;
};

extern uint32_t VarIndex;
extern uint32_t LabelIndex;

// Temp
//  - llvm size
//  - tcg size
//
// Vector
//  - element size
//  - number elements
//
// Runtime:
//   - uint32_t Id
//   - std::string ConstantExpr
// 

using LlvmScalarSize = uint8_t;

struct TcgScalarSize {
    TcgSize Tcg;
    LlvmScalarSize Llvm;
};

struct TcgVectorSize {
  LlvmScalarSize ElementCount;
  LlvmScalarSize ElemenSize;
};

struct TcgScalar {
  uint32_t Id;
  TcgScalarSize TcgSize;
  LlvmScalarSize LvmSize;
  TcgVKind Kind;
};

struct TcgVector {
  uint32_t Id;
  LlvmScalarSize ElementCount;
  LlvmScalarSize ElementSize;
  TcgVKind Kind;
};

struct TcgV {
  uint32_t Id;
  std::string Name;

  uint32_t TcgSize;
  uint32_t LlvmSize;

  uint32_t VectorElementCount;

  TcgVKind Kind;

  bool ConstantExpression = false;

  static TcgV makeVector(uint32_t VectorWidthBits, 
                         uint32_t ElementWidthBits,
                         uint32_t ElementCount) {
    return TcgV("", VectorWidthBits, ElementWidthBits, ElementCount, IrPtrToOffset);
  }

  static TcgV makeImmediate(llvm::StringRef Name,
                            uint32_t TcgWidth,
                            uint32_t LlvmWidth) {
    return TcgV(Name.str(), TcgWidth, LlvmWidth, 1, IrImmediate);
  }

  static TcgV makeTemp(uint32_t TcgWidth,
                       uint32_t LlvmWidth, 
                       TcgVKind Kind) {
    return TcgV("", TcgWidth, LlvmWidth, 1, Kind);
  }

  static TcgV makeConstantExpression(llvm::StringRef Expression,
                                     uint32_t TcgWidth,
                                     uint32_t LlvmWidth, 
                                     TcgVKind Kind) {
    TcgV Tcg(Expression.str(), TcgWidth, LlvmWidth, 1, Kind);
    Tcg.ConstantExpression = true;
    return Tcg;
  }

  static TcgV makeLabel() {
    return TcgV("", 32, 32, 1, IrLabel);
  }

  TcgV(std::string Name, uint32_t TcgSize, uint32_t LlvmSize,
       uint32_t VectorElementCount, TcgVKind Kind)
    : Id(VarIndex++), Name(Name), TcgSize(TcgSize), LlvmSize(LlvmSize),
    VectorElementCount(VectorElementCount), Kind(Kind) {
    assert(verifySize());
  }

  // We make the following assumptions about TcgSize and LLvmSize:
  //   - TcgSize either 32- or 64-bit;
  //   - LlvmSize either 1-,8-,16-,32-,64-,or 128-bit.
  // We also assume that there are only these valid combinations of
  // (TcgSize, LlvmSize):
  //   - (64, 64) uint64_t
  //   - (64, 1)  bool
  //   - (32, 32) uint32_t
  //   - (32, 16) uint16_t
  //   - (32, 8)  uint8_t
  //   - (32, 1)  bool
  // So we try to fit the variables in the smallest possible TcgSize,
  // with the exception of booleans which need to able to be 64-bit
  // when dealing with conditions.
  bool verifySize() {
    assert(LlvmSize <= TcgSize);
    return (TcgSize == 64 || TcgSize == 32) && (LlvmSize <= TcgSize);
    //return (TcgSize == 64 && (LlvmSize == 64  ||
    //                          LlvmSize == 32  ||
    //                          LlvmSize == 16  ||
    //                          LlvmSize == 8   ||
    //                          LlvmSize == 1)) ||
    //       (TcgSize == 32 && (LlvmSize == 32 ||
    //                          LlvmSize == 16 ||
    //                          LlvmSize == 8  ||
    //                          LlvmSize == 1));
  }

  // TODO(anjo): Update when we move to indexes instead of storing name here
  bool operator==(const TcgV &Other) const {
    return Other.Id == Id;
  }

  bool operator!=(const TcgV &Other) const {
    return !operator==(Other);
  }
};

inline uint64_t llvmToTcgSize(uint64_t LlvmSize) {
    return (LlvmSize <= 32)  ? 32 : 64;
}

inline uint32_t vectorSizeInBytes(const TcgV &Vec) {
  assert(Vec.Kind == IrPtrToOffset);
  return Vec.LlvmSize * Vec.VectorElementCount / 8;
}

struct TcgBinOp {
  std::string Code;
};

struct TcgVecBinOp {
  std::string Code;
  llvm::Optional<uint32_t> RequiredOp2Size;
};
