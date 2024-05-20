#include "PseudoInst.h"

#include "llvm/IR/Function.h"
#include "llvm/ADT/Twine.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/Casting.h"
#include "llvm/IR/Instructions.h"

#include <utils/Common.h>

#define PSEUDO_INST_DEF(name, ret, args) #name
static const char *PseudoInstName[] = {
#include "PseudoInst.inc"
};
#undef PSEUDO_INST_DEF

using namespace llvm;

// In order to map from a Function * to a PseudoInst, we keep a map
// of all Functions created, this simplifies mapping of callee's to
// a PseudoInst that can be switched over.
static DenseMap<Function *, PseudoInst> MapFuncToInst;

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
inline std::string getMangledTypeStr(llvm::Type *Ty) {
  std::string TypeStr = "";
  llvm::raw_string_ostream TypeStream(TypeStr);
  if (auto ArrayTy = llvm::dyn_cast<llvm::ArrayType>(Ty)) {
    std::string ElementStr = getMangledTypeStr(ArrayTy->getElementType());
    TypeStream << "a" << ArrayTy->getNumElements() << "x" << ElementStr;
  } else if (auto VecTy = llvm::dyn_cast<llvm::VectorType>(Ty)) {
    uint32_t ElementCount = getVectorElementCount(VecTy);
    std::string ElementStr = getMangledTypeStr(VecTy->getElementType());
    TypeStream << "v" << ElementCount << "x" << ElementStr;
  } else if (auto *StructTy = llvm::dyn_cast<llvm::StructType>(Ty)) {
    TypeStream << StructTy->getName();
  } else if (auto *IntTy = llvm::dyn_cast<llvm::IntegerType>(Ty)) {
    TypeStream << "i" << IntTy->getBitWidth();
  } else if (auto *PtrTy = llvm::dyn_cast<llvm::PointerType>(Ty)){
    std::string ElementStr = getMangledTypeStr(PtrTy->getPointerElementType());
    TypeStream << "p" << ElementStr;
  } else {
    abort();
  }

  return TypeStream.str();
}

llvm::FunctionCallee pseudoInstFunction(llvm::Module &M,
                                        PseudoInst Inst,
                                        llvm::Type *RetType,
                                        llvm::ArrayRef<llvm::Type *> ArgTypes) {
  auto FT = llvm::FunctionType::get(RetType, ArgTypes, false);

  std::string FnName{PseudoInstName[Inst]}; 
  if (!RetType->isVoidTy()) {
    FnName += ".";
    FnName += getMangledTypeStr(RetType);
  }
  for (llvm::Type *Ty : ArgTypes) {
    if (Ty->isLabelTy()) {
      continue;
    }
    FnName += ".";
    FnName += getMangledTypeStr(Ty);
  }

  llvm::FunctionCallee Fn = M.getOrInsertFunction(FnName, FT);
  llvm::errs() << *Fn.getCallee() << "\n";
  auto *F = llvm::cast<llvm::Function>(Fn.getCallee());
  MapFuncToInst.insert({F, Inst});

  return Fn;
}

// Takes value as convenience
Expected<PseudoInst> getPseudoInstFromCall(CallInst *Call) {
  Function *F = Call->getCalledFunction();
  auto It = MapFuncToInst.find(F);
  if (It == MapFuncToInst.end()) {
    return mkError("Unrecognized pseudo inst: ", Call);
  }
  return It->second;
}
