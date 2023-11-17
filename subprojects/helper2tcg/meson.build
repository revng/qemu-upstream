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
#llvm_dep  = dependency('llvm', version: ['>= 10', '<= 14'])
#opt       = find_program(llvm_dep.get_variable(configtool: 'bindir') / 'opt')
#clang     = find_program(llvm_dep.get_variable(configtool: 'bindir') / 'clang')
#llvm_link = find_program(llvm_dep.get_variable(configtool: 'bindir') / 'llvm-link')

#
# Hardcode LLVM paths for testing specific versions, obviously not good
#

# llvm 14
#llvm_dep  = meson.get_compiler('c').find_library('LLVM-14')
#opt       = find_program('/usr/lib/llvm14/bin/opt')
#clang     = find_program('/usr/lib/llvm14/bin/clang')
#llvm_link = find_program('/usr/lib/llvm14/bin/llvm-link')
#include_path = '/usr/lib/llvm14/include'

# llvm 13
llvm_dep  = meson.get_compiler('c').find_library('LLVM-13', dirs: '/home/aj/git/system/llvm/versions/13/lib')
opt       = find_program('/home/aj/git/system/llvm/versions/13/bin/opt')
clang     = find_program('/home/aj/git/system/llvm/versions/13/bin/clang')
lvm_link = find_program('/home/aj/git/system/llvm/versions/13/bin/llvm-link')
include_path = '/home/aj/git/system/llvm/versions/13/include'

# llvm 12
#llvm_dep  = meson.get_compiler('c').find_library('LLVM-12', dirs: '/home/aj/git/system/llvm/versions/12/lib')
#opt       = find_program('/home/aj/git/system/llvm/versions/12/bin/opt')
#clang     = find_program('/home/aj/git/system/llvm/versions/12/bin/clang')
#llvm_link = find_program('/home/aj/git/system/llvm/versions/12/bin/llvm-link')
#include_path = '/home/aj/git/system/llvm/versions/12/include'

# llvm 11
#llvm_dep  = meson.get_compiler('c').find_library('LLVM-11', dirs: '/home/aj/git/system/llvm/versions/11/lib')
#opt       = find_program('/home/aj/git/system/llvm/versions/11/bin/opt')
#clang     = find_program('/home/aj/git/system/llvm/versions/11/bin/clang')
#llvm_link = find_program('/home/aj/git/system/llvm/versions/11/bin/llvm-link')
#include_path = '/home/aj/git/system/llvm/versions/11/include'

# llvn 10
#llvm_dep  = meson.get_compiler('c').find_library('LLVM-10', dirs: '/home/aj/git/system/llvm/versions/10/lib')
#opt       = find_program('/home/aj/git/system/llvm/versions/10/bin/opt')
#clang     = find_program('/home/aj/git/system/llvm/versions/10/bin/clang')
#llvm_link = find_program('/home/aj/git/system/llvm/versions/10/bin/llvm-link')
#include_path = '/home/aj/git/system/llvm/versions/10/include'

get_llvm_ir = generator(python,
                        output: '@BASENAME@.ll',
                        arguments: [meson.current_source_dir() / 'get-llvm-ir.py',
                                    clang.full_path(), '@OUTPUT@', '@INPUT@',
                                    '@EXTRA_ARGS@'])

pipeline = executable('pipeline', sources,
                      include_directories: ['passes', './', include_path],
                      dependencies: llvm_dep,
                      cpp_args: cpp_args)
