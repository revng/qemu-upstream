sources = [
    'scalar.c',
    'vector.c',
    'ldst.c',
    'cpustate.c',
]


foreach s : sources
    name = s.split('.')[0]
    name_ll = name + '.ll'
    ll = custom_target(name_ll,
        input: s,
        output: name_ll,
        command: [clang, '-O0', '-Xclang', '-disable-O0-optnone',
                  '-S', '-emit-llvm', '-o', '@OUTPUT@', '@INPUT@']
    )
    test(name, pipeline,
        args: [ll,
               '--mmu-index-function=tb_mmu_index',
               '--tcg-global-mappings=mappings',
               '--translate-all-helpers'],
        suite: 'end-to-end')
endforeach
