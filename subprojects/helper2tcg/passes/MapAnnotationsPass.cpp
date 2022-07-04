#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instruction.h"
#include <llvm/IR/Instructions.h>
#include <utils/Common.h>

#include "MapAnnotationsPass.h"

using namespace llvm;

static Expected<Annotation> parseAnnotationStr(StringRef Str,
                                               uint32_t num_function_args) {
  Annotation Ann;

  Str = Str.trim();

  if (Str.consume_front("llvm-to-tcg")) {
    Ann.Kind = AnnLlvmToTcg;
    return Ann;
  } else if (Str.consume_front("immediate")) {
    Ann.Kind = AnnImmediate;
  } else if (Str.consume_front("ptr-to-offset")) {
    Ann.Kind = AnnPtrToOffset;
  } else {
    return mkError("Unknown annotation");
  }
  
  if (!Str.consume_front(":")) {
    return mkError("Expected \":\"");
  }

  Str = Str.ltrim(' ');
  do {
    Str = Str.ltrim(' ');
    uint32_t i = 0;
    Str.consumeInteger(10, i);
    if (i >= num_function_args) {
      return mkError("Annotation has out of bounds argument index");
    }
    Ann.ArgIndices.push_back(i);
  } while (Str.consume_front(","));

  return Ann;
}

MapAnnotationsPass::Result MapAnnotationsPass::run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM) {
  // We're using cast over dyn_cast here, so we're
  // essentially asserting that the structure of
  //
  //     llvm.global.annotation
  //
  // is what we expect.
  Result AnnotationMap;
  GlobalVariable *GA = M.getGlobalVariable("llvm.global.annotations");
  if (!GA) {
    return AnnotationMap;
  }

  // Get the metadata which is stored in the first op
  auto *CA = cast<ConstantArray>(GA->getOperand(0));
  // Loop over metadata
  for (Value *CAOp : CA->operands()) {
    auto *Struct = cast<ConstantStruct>(CAOp);
    assert(Struct->getNumOperands() >= 2);
    Constant *UseOfF = Struct->getOperand(0);
    if (isa<UndefValue>(UseOfF)) {
      continue;
    }
    auto *F = cast<Function>(UseOfF->getOperand(0));
    auto *AnnVar = cast<GlobalVariable>(Struct->getOperand(1)->getOperand(0));
    auto *AnnData = cast<ConstantDataArray>(AnnVar->getOperand(0));

    StringRef AnnStr = AnnData->getAsString();
    AnnStr = AnnStr.substr(0, AnnStr.size()-1);
    Expected<Annotation> Ann = parseAnnotationStr(AnnStr, F->arg_size());
    if (!Ann) {
      dbgs() << "Failed to parse annotation: \"" << Ann.takeError()
             << "\" for function " << F->getName() << "\n";
      continue;
    }
    AnnotationMap.Map[F].push_back(*Ann);
  }

  return AnnotationMap;
}

AnalysisKey MapAnnotationsPass::Key{};
