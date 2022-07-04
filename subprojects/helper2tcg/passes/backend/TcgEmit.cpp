#include "TcgEmit.h"
#include "llvm/IR/Instructions.h"

uint32_t VarIndex = 0;
uint32_t LabelIndex = 0;
uint32_t NumTempVectorOffsets = 0;

namespace tcg {

using namespace llvm;

void resetNameIndices() {
  VarIndex = 0;
  LabelIndex = 0;
  NumTempVectorOffsets = 0;
}

const std::string mkName(const std::string Suffix) {
  return Twine("v")
    .concat(Suffix)
    .concat("_")
    .concat(Twine(VarIndex++))
    .str();
}

const std::string getType(const TcgV &Value) {
    switch (Value.Kind) {
    case IrValue:
    case IrConst:
      return Twine("TCGv_i").concat(Twine(Value.TcgSize)).str();
    case IrEnv:
      return "TCGv_env";
    case IrImmediate:
      if (Value.LlvmSize == 1)
        return "bool";
      else
        return Twine("int").concat(Twine(Value.LlvmSize)).concat("_t").str();
    case IrPtr:
      return "TCGv_ptr";
    case IrPtrToOffset:
      return "intptr_t";
    case IrLabel:
      return "TCGLabel *";
    default:
      assert(false);
    }
}

}
