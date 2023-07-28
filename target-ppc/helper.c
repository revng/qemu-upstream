#include "cpu.h"
CPUState *cpu_create(void)
{
    PowerPCCPU *cpu = g_malloc0(sizeof(PowerPCCPU));
    CPUState *cs = CPU(cpu);
    memcpy(cpu, POWERPC_CPU(first_cpu), sizeof(PowerPCCPU));
    cs->env_ptr = &cpu->env;
    return cs;
}
