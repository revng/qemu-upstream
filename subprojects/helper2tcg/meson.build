project('helper2tcg', ['c', 'cpp'], meson_version: '>=0.63.0', version: '0.5')

python = import('python').find_installation()

cpp_args = ['-std=c++17', '-fno-rtti', '-fno-exceptions', '-O0','-g', '-DLLVM_DISABLE_ABI_BREAKING_CHECKS_ENFORCING=1']
sources = [
    'pipeline/Pipeline.cpp',
    'passes/RemoveNoinlinePass.cpp',
    'passes/FilterFunctionsPass.cpp',
    'passes/FinalFilterFunctionsPass.cpp',
    'passes/MapAnnotationsPass.cpp',
    'passes/TcgTempAllocationPass.cpp',
    'passes/TcgGlobalMapPass.cpp',
    'passes/RemovePhisPass.cpp',
    'passes/TcgInstCombinePass.cpp',
    'passes/IdMapPass.cpp',
    'passes/MapGEPPass.cpp',
    'passes/backend/TcgGenPass.cpp',
    'passes/backend/TcgEmit.cpp',
    'passes/llvm-compat.cpp',
    'passes/PseudoInst.cpp',
  ]

# Here's the LLVM dep that should autodetect LLVM versions, note this does not always appear to
# work when multiple versions of LLVM are installed on the same system. It appears to just list
# them in _some_ order and pick the first one. So even if you have LLVM 14 installed, it might
# not find it... Great.
llvm_dep  = dependency('llvm', version: ['>= 10', '< 15'])
opt       = find_program(llvm_dep.get_variable(configtool: 'bindir') / 'opt')
clang     = find_program(llvm_dep.get_variable(configtool: 'bindir') / 'clang')
llvm_link = find_program(llvm_dep.get_variable(configtool: 'bindir') / 'llvm-link')
link_args = llvm_dep.get_variable(configtool: 'ldflags')

get_llvm_ir = generator(python,
                        output: '@BASENAME@.ll',
                        arguments: [meson.current_source_dir() / 'get-llvm-ir.py',
                                    clang.full_path(), '@OUTPUT@', '@INPUT@',
                                    '@EXTRA_ARGS@'])

pipeline = executable('pipeline', sources,
                      include_directories: ['passes', './'],
                      dependencies: llvm_dep,
                      link_args: link_args,
                      cpp_args: cpp_args)
