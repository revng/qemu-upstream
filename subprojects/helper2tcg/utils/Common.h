#pragma once

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Error.h"

inline llvm::Error mkError(const llvm::StringRef Msg) {
  return llvm::createStringError(llvm::inconvertibleErrorCode(), Msg);
}

inline llvm::Error mkError(const llvm::StringRef Msg, const llvm::Value *V) {
  std::string Str;
  llvm::raw_string_ostream(Str) << Msg << *V;
  return llvm::createStringError(llvm::inconvertibleErrorCode(), Str);
}

// TODO(anjo): Move to compat
inline uint32_t getVectorElementCount(llvm::VectorType *VecTy) {
  auto ElementCount = VecTy->getElementCount();
#if LLVM_VERSION_MAJOR > 11
  return ElementCount.getFixedValue();
#else
  return ElementCount.Min;
#endif
}

//
// Converts llvm `Type`s to a string representation
// that can be embedded in function names for basic overlaoding.
//
// Ex.
//
//      *i32 -> "pi32"
//      *[8 x i8] -> "pa8xi8"
//      *<128 x i8> -> "pv128xi8"
//      *struct.CPUArchState -> "pstruct.CPUArchState"
//
// LLVM has an implementation of a similar function used by intrinsics,
// called getMangledTypeStr, but it's not exposed.
inline std::string appendTypeToTwine(llvm::Type *Ty) {
  std::string TypeStr = "";
  llvm::raw_string_ostream TypeStream(TypeStr);
  if (auto ArrayTy = llvm::dyn_cast<llvm::ArrayType>(Ty)) {
    std::string ElementStr = appendTypeToTwine(ArrayTy->getElementType());
    TypeStream << "a" << ArrayTy->getNumElements() << "x" << ElementStr;
  } else if (auto VecTy = llvm::dyn_cast<llvm::VectorType>(Ty)) {
    uint32_t ElementCount = getVectorElementCount(VecTy);
    std::string ElementStr = appendTypeToTwine(VecTy->getElementType());
    TypeStream << "v" << ElementCount << "x" << ElementStr;
  } else if (auto *StructTy = llvm::dyn_cast<llvm::StructType>(Ty)) {
    TypeStream << StructTy->getName();
  } else if (auto *IntTy = llvm::dyn_cast<llvm::IntegerType>(Ty)) {
    TypeStream << "i" << IntTy->getBitWidth() << "\n";
  } else if (auto *PtrTy = llvm::dyn_cast<llvm::PointerType>(Ty)){
    std::string ElementStr = appendTypeToTwine(PtrTy->getPointerElementType());
    TypeStream << "p" << ElementStr;
  } else {
    abort();
  }

  return TypeStream.str();
}
