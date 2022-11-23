#include "RemoveNoinlinePass.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Transforms/Utils/Local.h"

using namespace llvm;

PreservedAnalyses RemoveNoinlinePass::run(Function &F, FunctionAnalysisManager &FAM) {
  F.removeFnAttr(Attribute::AttrKind::NoInline);
  return PreservedAnalyses::all();
}
