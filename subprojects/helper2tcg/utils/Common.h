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
