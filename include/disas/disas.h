#ifndef QEMU_DISAS_H
#define QEMU_DISAS_H

#include "exec/hwaddr.h"
#include "exec/cpu-common.h"
#include "hw/core/cpu.h"

typedef uint64_t tb_page_addr_t;

/* Disassemble this for me please... (debugging). */
void disas(FILE *out, const void *code, unsigned long size);
void target_disas(FILE *out, CPUState *cpu, tb_page_addr_t code,
                  tb_page_addr_t size);

void monitor_disas(Monitor *mon, CPUState *cpu,
                   tb_page_addr_t pc, int nb_insn, int is_physical);

char *plugin_disas(CPUState *cpu, uint64_t addr, size_t size);

/* Look up symbol for debugging purpose.  Returns "" if unknown. */
const char *lookup_symbol(tb_page_addr_t orig_addr);

struct syminfo;
struct elf32_sym;
struct elf64_sym;

#if defined(CONFIG_USER_ONLY)
typedef const char *(*lookup_symbol_t)(struct syminfo *s, uint64_t orig_addr);
#else
typedef const char *(*lookup_symbol_t)(struct syminfo *s, hwaddr orig_addr);
#endif

struct syminfo {
    lookup_symbol_t lookup_symbol;
    unsigned int disas_num_syms;
    union {
      struct elf32_sym *elf32;
      struct elf64_sym *elf64;
    } disas_symtab;
    const char *disas_strtab;
    struct syminfo *next;
};

/* Filled in by elfload.c.  Simplistic, but will do for now. */
extern struct syminfo *syminfos;

#endif /* QEMU_DISAS_H */
