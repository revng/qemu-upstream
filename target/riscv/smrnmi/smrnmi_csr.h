
#define CSR_MNCAUSE 0x742
#define CSR_MNEPC 0x741
#define CSR_MNSCRATCH 0x740
#define CSR_MNSTATUS 0x744
#define MNCAUSE_INT 0x80000000
#define MNCAUSE_CODE 0x7fffffff
#define MNEPC_PC 0xffffffff
#define MNSCRATCH_SCRATCH 0xffffffff
#define MNSTATUS_MNPP 0x1800
#define MNSTATUS_MNPV 0x80
#define MNSTATUS_NMIE 0x8
void smrnmi_register_custom_csrs(RISCVCPU *cpu);
