#include "TcgGlobalMapPass.h"

#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/ADT/SmallVector.h"

using namespace llvm;

TcgGlobalMapPass::Result TcgGlobalMapPass::run(Module &M, ModuleAnalysisManager &MAM) {
  Result OffsetMap{};

  // TODO(anjo): Add ability to configure this
  auto Map = M.getGlobalVariable("tcg_global_mappings");
  if (!Map)
    return OffsetMap;

  // In case the `tcg_global_mappings` array is empty,
  // casting to `ConstantArray` will fail, even though it's a
  // `[0 x %struct.cpu_tcg_mapping]`.
  auto MapElems = dyn_cast<ConstantArray>(Map->getOperand(0));
  if (!MapElems)
    return OffsetMap;

  for (auto Row : MapElems->operand_values()) {
    auto ConstRow = cast<ConstantStruct>(Row);

    // Get code string
    auto CodePtr = ConstRow->getOperand(0);
    auto CodeStr = cast<ConstantDataArray>(cast<Constant>(CodePtr->getOperand(0))->getOperand(0))->getAsString();
    CodeStr = CodeStr.rtrim('\0');

    // Get offset in cpu env
    auto Offset = cast<ConstantInt>(ConstRow->getOperand(3));
    // Get size of variable in cpu env
    auto SizeInBytes = cast<ConstantInt>(ConstRow->getOperand(4));
    auto SizeInBits = 8*SizeInBytes->getLimitedValue();

    auto Stride = cast<ConstantInt>(ConstRow->getOperand(5));
    auto NumElements = cast<ConstantInt>(ConstRow->getOperand(6));

    OffsetMap[Offset->getLimitedValue()] = {
      CodeStr,
      SizeInBits,
      NumElements->getLimitedValue(),
      Stride->getLimitedValue()
    };
  }

  return OffsetMap;
}

llvm::AnalysisKey TcgGlobalMapPass::Key{};
