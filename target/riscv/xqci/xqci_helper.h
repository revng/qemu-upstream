#ifdef TARGET_RISCV32
DEF_HELPER_4(xqci_swm, void, env, tl, tl, s32)
DEF_HELPER_4(xqci_lwm, void, env, tl, tl, s32)
DEF_HELPER_4(xqci_setwm, void, env, tl, tl, tl)

DEF_HELPER_1(xqci_mienter, void, env)
DEF_HELPER_1(xqci_mienter_nest, void, env)
DEF_HELPER_1(xqci_mileaveret, void, env)

DEF_HELPER_3(xqci_outw, void, env, tl, tl)
DEF_HELPER_2(xqci_inw, tl, env, tl)
#endif
