#pragma once

//
// The purpose of this file is to both collect and hide most api-specific changes
// of LLVM [10,14]. Hopefully making it easier to keep track of the changes necessary
// to support our targeted versions.
//
// Note some #ifdefs still remain throughout the codebase for larger codeblocks that are
// specific enough such that pulling them here would be more cumbersome than it's worth.
//

#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"

#if LLVM_VERSION_MAJOR > 11
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"
#endif

#if LLVM_VERSION_MAJOR >= 14
#include "llvm/MC/TargetRegistry.h"
#else
#include "llvm/Support/TargetRegistry.h"
#endif

#include "llvm/IR/PatternMatch.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Passes/OptimizationLevel.h"
#include "llvm/Passes/PassBuilder.h"

namespace compat {

#if LLVM_VERSION_MAJOR == 14 || LLVM_VERSION_MAJOR == 13
  constexpr auto OpenFlags = llvm::sys::fs::OF_TextWithCRLF;
#else
  constexpr auto OpenFlags = llvm::sys::fs::OF_Text;
#endif

#if LLVM_VERSION_MAJOR == 14
  using OptimizationLevel = llvm::OptimizationLevel;
#else
  using OptimizationLevel = llvm::PassBuilder::OptimizationLevel;
#endif

#if LLVM_VERSION_MAJOR > 11
  constexpr auto LTOPhase = llvm::ThinOrFullLTOPhase::None;
#else
  constexpr auto LTOPhase = llvm::PassBuilder::ThinLTOPhase::None;
#endif

  inline llvm::PassBuilder createPassBuilder(llvm::TargetMachine *TM,
                                             llvm::PipelineTuningOptions &PTO) {
#if LLVM_VERSION_MAJOR == 14 || LLVM_VERSION_MAJOR == 13
    return llvm::PassBuilder(TM, PTO, llvm::None);
#elif LLVM_VERSION_MAJOR == 12
    return llvm::PassBuilder(TM, nullptr, PTO);
#else
    return llvm::PassBuilder(TM, PTO);
#endif
  }

// Wrapper to convert Function- to Module analysis manager
template<typename T>
inline const typename T::Result *getModuleAnalysisManagerProxyResult(llvm::FunctionAnalysisManager &FAM, llvm::Function &F) {
#if LLVM_VERSION_MAJOR > 10
  auto &MAMProxy = FAM.getResult<llvm::ModuleAnalysisManagerFunctionProxy>(F);
  return MAMProxy.getCachedResult<T>(*F.getParent());
#else
  auto &MAMProxy = FAM.getResult<llvm::ModuleAnalysisManagerFunctionProxy>(F).getManager();
  return MAMProxy.getCachedResult<T>(*F.getParent());
#endif
}

llvm::TargetMachine *getTargetMachine(llvm::Triple &TheTriple);

//
// LLVM 11 and below does not define the UnifyFunctionExitNodes pass
// for the new pass manager.  Copy over the definition and use it for
// 11 and below.
//
#if LLVM_VERSION_MAJOR > 11
using llvm::UnifyFunctionExitNodesPass;
#else
class UnifyFunctionExitNodesPass : public llvm::PassInfoMixin<UnifyFunctionExitNodesPass> {
public:
  llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &AM);
};
#endif

//
// PatternMatch
//

#if LLVM_VERSION_MAJOR > 10
#define compat_m_InsertElt llvm::PatternMatch::m_InsertElt
#define compat_m_Shuffle   llvm::PatternMatch::m_Shuffle
#define compat_m_ZeroMask  llvm::PatternMatch::m_ZeroMask
#else
#define compat_m_InsertElt llvm::PatternMatch::m_InsertElement
#define compat_m_Shuffle   llvm::PatternMatch::m_ShuffleVector
#define compat_m_ZeroMask  llvm::PatternMatch::m_Zero
#endif

}
