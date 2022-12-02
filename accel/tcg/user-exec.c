/*
 *  User emulator execution
 *
 *  Copyright (c) 2003-2005 Fabrice Bellard
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */
#include "qemu/osdep.h"
#include "hw/core/tcg-cpu-ops.h"
#include "disas/disas.h"
#include "tcg/tcg.h"
#include "qemu/bitops.h"
#include "exec/translate-all.h"
#include "exec/helper-proto.h"
#include "qemu/atomic128.h"
#include "trace/trace-root.h"
#include "internal.h"

CPUState *env_cpu(CPUArchState *env);

// WIP: cpu-ldst.h
bool guest_addr_valid_untagged(uint64_t x);
void set_helper_retaddr(uintptr_t ra);
uint8_t cpu_ldb_mmu(CPUArchState *env, uint64_t ptr, MemOpIdx oi, uintptr_t ra);
void clear_helper_retaddr(void);
uint16_t cpu_ldw_be_mmu(CPUArchState *env, uint64_t ptr,
                        MemOpIdx oi, uintptr_t ra);
uint32_t cpu_ldl_be_mmu(CPUArchState *env, uint64_t ptr,
                        MemOpIdx oi, uintptr_t ra);
uint64_t cpu_ldq_be_mmu(CPUArchState *env, uint64_t ptr,
                        MemOpIdx oi, uintptr_t ra);
uint16_t cpu_ldw_le_mmu(CPUArchState *env, uint64_t ptr,
                        MemOpIdx oi, uintptr_t ra);
uint32_t cpu_ldl_le_mmu(CPUArchState *env, uint64_t ptr,
                        MemOpIdx oi, uintptr_t ra);
uint64_t cpu_ldq_le_mmu(CPUArchState *env, uint64_t ptr,
                        MemOpIdx oi, uintptr_t ra);
void cpu_stb_mmu(CPUArchState *env, uint64_t ptr, uint8_t val,
                 MemOpIdx oi, uintptr_t ra);
void cpu_stw_be_mmu(CPUArchState *env, uint64_t ptr, uint16_t val,
                    MemOpIdx oi, uintptr_t ra);
void cpu_stl_be_mmu(CPUArchState *env, uint64_t ptr, uint32_t val,
                    MemOpIdx oi, uintptr_t ra);
void cpu_stq_be_mmu(CPUArchState *env, uint64_t ptr, uint64_t val,
                    MemOpIdx oi, uintptr_t ra);
void cpu_stw_le_mmu(CPUArchState *env, uint64_t ptr, uint16_t val,
                    MemOpIdx oi, uintptr_t ra);
void cpu_stl_le_mmu(CPUArchState *env, uint64_t ptr, uint32_t val,
                    MemOpIdx oi, uintptr_t ra);
void cpu_stq_le_mmu(CPUArchState *env, uint64_t ptr, uint64_t val,
                    MemOpIdx oi, uintptr_t ra);
uint32_t cpu_ldub_code(CPUArchState *env, uint64_t addr);
uint32_t cpu_lduw_code(CPUArchState *env, uint64_t addr);
uint32_t cpu_ldl_code(CPUArchState *env, uint64_t addr);
uint64_t cpu_ldq_code(CPUArchState *env, uint64_t addr);

// WIP: from cpu-all.h
int page_get_flags(uint64_t address);
void page_set_flags(uint64_t start, uint64_t end, int flags);
void page_reset_target_data(uint64_t start, uint64_t end);
int page_check_range(uint64_t start, uint64_t len, int flags);

/* same as PROT_xxx */
#define PAGE_READ      0x0001
#define PAGE_WRITE     0x0002
#define PAGE_EXEC      0x0004
#define PAGE_BITS      (PAGE_READ | PAGE_WRITE | PAGE_EXEC)
#define PAGE_VALID     0x0008
/*
 * Original state of the write flag (used when tracking self-modifying code)
 */
#define PAGE_WRITE_ORG 0x0010
/*
 * Invalidate the TLB entry immediately, helpful for s390x
 * Low-Address-Protection. Used with PAGE_WRITE in tlb_set_page_with_attrs()
 */
#define PAGE_WRITE_INV 0x0020
/* For use with page_set_flags: page is being replaced; target_data cleared. */
#define PAGE_RESET     0x0040
/* For linux-user, indicates that the page is MAP_ANON. */
#define PAGE_ANON      0x0080

#if defined(CONFIG_BSD) && defined(CONFIG_USER_ONLY)
/* FIXME: Code that sets/uses this is broken and needs to go away.  */
#define PAGE_RESERVED  0x0100
#endif
/* Target-specific bits that will be used via page_get_flags().  */
#define PAGE_TARGET_1  0x0200
#define PAGE_TARGET_2  0x0400

/*
 * For linux-user, indicates that the page is mapped with the same semantics
 * in both guest and host.
 */
#define PAGE_PASSTHROUGH 0x0800

#define TLB_INVALID_MASK    (1 << (/* TARGET_PAGE_BITS_MIN */10 - 1))
#define TLB_MMIO            0
#define TLB_WATCHPOINT      0

typedef int (*walk_memory_regions_fn)(void *, uint64_t,
                                      uint64_t, unsigned long);
int walk_memory_regions(void *, walk_memory_regions_fn);
void page_dump(FILE *f);

// WIP: from tcg/tcg-ldst.h
void helper_unaligned_ld(CPUArchState *env, uint64_t addr);
void helper_unaligned_st(CPUArchState *env, uint64_t addr);

__thread uintptr_t helper_retaddr;

//#define DEBUG_SIGNAL

/*
 * Adjust the pc to pass to cpu_restore_state; return the memop type.
 */
MMUAccessType adjust_signal_pc(uintptr_t *pc, bool is_write)
{
    switch (helper_retaddr) {
    default:
        /*
         * Fault during host memory operation within a helper function.
         * The helper's host return address, saved here, gives us a
         * pointer into the generated code that will unwind to the
         * correct guest pc.
         */
        *pc = helper_retaddr;
        break;

    case 0:
        /*
         * Fault during host memory operation within generated code.
         * (Or, a unrelated bug within qemu, but we can't tell from here).
         *
         * We take the host pc from the signal frame.  However, we cannot
         * use that value directly.  Within cpu_restore_state_from_tb, we
         * assume PC comes from GETPC(), as used by the helper functions,
         * so we adjust the address by -GETPC_ADJ to form an address that
         * is within the call insn, so that the address does not accidentally
         * match the beginning of the next guest insn.  However, when the
         * pc comes from the signal frame it points to the actual faulting
         * host memory insn and not the return from a call insn.
         *
         * Therefore, adjust to compensate for what will be done later
         * by cpu_restore_state_from_tb.
         */
        *pc += GETPC_ADJ;
        break;

    case 1:
        /*
         * Fault during host read for translation, or loosely, "execution".
         *
         * The guest pc is already pointing to the start of the TB for which
         * code is being generated.  If the guest translator manages the
         * page crossings correctly, this is exactly the correct address
         * (and if the translator doesn't handle page boundaries correctly
         * there's little we can do about that here).  Therefore, do not
         * trigger the unwinder.
         */
        *pc = 0;
        return MMU_INST_FETCH;
    }

    return is_write ? MMU_DATA_STORE : MMU_DATA_LOAD;
}

/**
 * handle_sigsegv_accerr_write:
 * @cpu: the cpu context
 * @old_set: the sigset_t from the signal ucontext_t
 * @host_pc: the host pc, adjusted for the signal
 * @guest_addr: the guest address of the fault
 *
 * Return true if the write fault has been handled, and should be re-tried.
 *
 * Note that it is important that we don't call page_unprotect() unless
 * this is really a "write to nonwritable page" fault, because
 * page_unprotect() assumes that if it is called for an access to
 * a page that's writable this means we had two threads racing and
 * another thread got there first and already made the page writable;
 * so we will retry the access. If we were to call page_unprotect()
 * for some other kind of fault that should really be passed to the
 * guest, we'd end up in an infinite loop of retrying the faulting access.
 */
bool handle_sigsegv_accerr_write(CPUState *cpu, sigset_t *old_set,
                                 uintptr_t host_pc, uint64_t guest_addr)
{
    switch (page_unprotect(guest_addr, host_pc)) {
    case 0:
        /*
         * Fault not caused by a page marked unwritable to protect
         * cached translations, must be the guest binary's problem.
         */
        return false;
    case 1:
        /*
         * Fault caused by protection of cached translation; TBs
         * invalidated, so resume execution.
         */
        return true;
    case 2:
        /*
         * Fault caused by protection of cached translation, and the
         * currently executing TB was modified and must be exited immediately.
         */
        sigprocmask(SIG_SETMASK, old_set, NULL);
        cpu_loop_exit_noexc(cpu);
        /* NORETURN */
    default:
        g_assert_not_reached();
    }
}

static int probe_access_internal(CPUArchState *env, uint64_t addr,
                                 int fault_size, MMUAccessType access_type,
                                 bool nonfault, uintptr_t ra)
{
    int acc_flag;
    bool maperr;

    switch (access_type) {
    case MMU_DATA_STORE:
        acc_flag = PAGE_WRITE_ORG;
        break;
    case MMU_DATA_LOAD:
        acc_flag = PAGE_READ;
        break;
    case MMU_INST_FETCH:
        acc_flag = PAGE_EXEC;
        break;
    default:
        g_assert_not_reached();
    }

    if (guest_addr_valid_untagged(addr)) {
        int page_flags = page_get_flags(addr);
        if (page_flags & acc_flag) {
            return 0; /* success */
        }
        maperr = !(page_flags & PAGE_VALID);
    } else {
        maperr = true;
    }

    if (nonfault) {
        return TLB_INVALID_MASK;
    }

    cpu_loop_exit_sigsegv(env_cpu(env), addr, access_type, maperr, ra);
}

int probe_access_flags(CPUArchState *env, uint64_t addr,
                       MMUAccessType access_type, int mmu_idx,
                       bool nonfault, void **phost, uintptr_t ra)
{
    int flags;

    flags = probe_access_internal(env, addr, 0, access_type, nonfault, ra);
#if 0
    *phost = flags ? NULL : g2h(env_cpu(env), addr);
#endif
    return flags;
}

void *probe_access(CPUArchState *env, uint64_t addr, int size,
                   MMUAccessType access_type, int mmu_idx, uintptr_t ra)
{
#if 0
    int flags;

    g_assert(-(addr | TARGET_PAGE_MASK) >= size);
    flags = probe_access_internal(env, addr, size, access_type, false, ra);
    g_assert(flags == 0);

    return size ? g2h(env_cpu(env), addr) : NULL;
#else
    abort();
#endif
}

tb_page_addr_t get_page_addr_code_hostp(CPUArchState *env, uint64_t addr,
                                        void **hostp)
{
    int flags;

    flags = probe_access_internal(env, addr, 1, MMU_INST_FETCH, false, 0);
    g_assert(flags == 0);

    if (hostp) {
#if 0
        *hostp = g2h_untagged(addr);
#endif
    }
    return addr;
}

void page_reset_target_data(uint64_t start, uint64_t end)
{
#ifdef TARGET_PAGE_DATA_SIZE
    uint64_t addr, len;

    /*
     * This function should never be called with addresses outside the
     * guest address space.  If this assert fires, it probably indicates
     * a missing call to h2g_valid.
     */
    assert(end - 1 <= GUEST_ADDR_MAX);
    assert(start < end);
    assert_memory_lock();

    start = start & TARGET_PAGE_MASK;
    end = TARGET_PAGE_ALIGN(end);

    for (addr = start, len = end - start;
         len != 0;
         len -= TARGET_PAGE_SIZE, addr += TARGET_PAGE_SIZE) {
        PageDesc *p = page_find_alloc(addr >> TARGET_PAGE_BITS, 1);

        g_free(p->target_data);
        p->target_data = NULL;
    }
#endif
}

#ifdef TARGET_PAGE_DATA_SIZE
void *page_get_target_data(uint64_t address)
{
    PageDesc *p = page_find(address >> TARGET_PAGE_BITS);
    void *ret = p->target_data;

    if (!ret) {
        ret = g_malloc0(TARGET_PAGE_DATA_SIZE);
        p->target_data = ret;
    }
    return ret;
}
#endif

/* The softmmu versions of these helpers are in cputlb.c.  */

/*
 * Verify that we have passed the correct MemOp to the correct function.
 *
 * We could present one function to target code, and dispatch based on
 * the MemOp, but so far we have worked hard to avoid an indirect function
 * call along the memory path.
 */
static void validate_memop(MemOpIdx oi, MemOp expected)
{
#ifdef CONFIG_DEBUG_TCG
    MemOp have = get_memop(oi) & (MO_SIZE | MO_BSWAP);
    assert(have == expected);
#endif
}

void helper_unaligned_ld(CPUArchState *env, uint64_t addr)
{
    cpu_loop_exit_sigbus(env_cpu(env), addr, MMU_DATA_LOAD, GETPC());
}

void helper_unaligned_st(CPUArchState *env, uint64_t addr)
{
    cpu_loop_exit_sigbus(env_cpu(env), addr, MMU_DATA_STORE, GETPC());
}

static void *cpu_mmu_lookup(CPUArchState *env, uint64_t addr,
                            MemOpIdx oi, uintptr_t ra, MMUAccessType type)
{
    MemOp mop = get_memop(oi);
    int a_bits = get_alignment_bits(mop);
    void *ret;

    /* Enforce guest required alignment.  */
    if (unlikely(addr & ((1 << a_bits) - 1))) {
        cpu_loop_exit_sigbus(env_cpu(env), addr, type, ra);
    }

#if 0
    ret = g2h(env_cpu(env), addr);
#endif
    set_helper_retaddr(ra);
    return ret;
}

uint8_t cpu_ldb_mmu(CPUArchState *env, uint64_t addr,
                    MemOpIdx oi, uintptr_t ra)
{
    void *haddr;
    (void) haddr;
    uint8_t ret;

    validate_memop(oi, MO_UB);
    haddr = cpu_mmu_lookup(env, addr, oi, ra, MMU_DATA_LOAD);
#if 0
    ret = ldub_p(haddr);
#else
    abort();
#endif
    clear_helper_retaddr();
    qemu_plugin_vcpu_mem_cb(env_cpu(env), addr, oi, QEMU_PLUGIN_MEM_R);
    return ret;
}

uint16_t cpu_ldw_be_mmu(CPUArchState *env, uint64_t addr,
                        MemOpIdx oi, uintptr_t ra)
{
    void *haddr;
    (void) haddr;
    uint16_t ret;

    validate_memop(oi, MO_BEUW);
    haddr = cpu_mmu_lookup(env, addr, oi, ra, MMU_DATA_LOAD);
#if 0
    ret = lduw_be_p(haddr);
#else
    abort();
#endif
    clear_helper_retaddr();
    qemu_plugin_vcpu_mem_cb(env_cpu(env), addr, oi, QEMU_PLUGIN_MEM_R);
    return ret;
}

uint32_t cpu_ldl_be_mmu(CPUArchState *env, uint64_t addr,
                        MemOpIdx oi, uintptr_t ra)
{
    void *haddr;
    (void) haddr;
    uint32_t ret;

    validate_memop(oi, MO_BEUL);
    haddr = cpu_mmu_lookup(env, addr, oi, ra, MMU_DATA_LOAD);
#if 0
    ret = ldl_be_p(haddr);
#else
    abort();
#endif
    clear_helper_retaddr();
    qemu_plugin_vcpu_mem_cb(env_cpu(env), addr, oi, QEMU_PLUGIN_MEM_R);
    return ret;
}

uint64_t cpu_ldq_be_mmu(CPUArchState *env, uint64_t addr,
                        MemOpIdx oi, uintptr_t ra)
{
    void *haddr;
    (void) haddr;
    uint64_t ret;

    validate_memop(oi, MO_BEUQ);
    haddr = cpu_mmu_lookup(env, addr, oi, ra, MMU_DATA_LOAD);
#if 0
    ret = ldq_be_p(haddr);
#else
    abort();
#endif
    clear_helper_retaddr();
    qemu_plugin_vcpu_mem_cb(env_cpu(env), addr, oi, QEMU_PLUGIN_MEM_R);
    return ret;
}

uint16_t cpu_ldw_le_mmu(CPUArchState *env, uint64_t addr,
                        MemOpIdx oi, uintptr_t ra)
{
    void *haddr;
    (void) haddr;
    uint16_t ret;

    validate_memop(oi, MO_LEUW);
    haddr = cpu_mmu_lookup(env, addr, oi, ra, MMU_DATA_LOAD);
#if 0
    ret = lduw_le_p(haddr);
#else
    abort();
#endif
    clear_helper_retaddr();
    qemu_plugin_vcpu_mem_cb(env_cpu(env), addr, oi, QEMU_PLUGIN_MEM_R);
    return ret;
}

uint32_t cpu_ldl_le_mmu(CPUArchState *env, uint64_t addr,
                        MemOpIdx oi, uintptr_t ra)
{
    void *haddr;
    (void) haddr;
    uint32_t ret;

    validate_memop(oi, MO_LEUL);
    haddr = cpu_mmu_lookup(env, addr, oi, ra, MMU_DATA_LOAD);
#if 0
    ret = ldl_le_p(haddr);
#else
    abort();
#endif
    clear_helper_retaddr();
    qemu_plugin_vcpu_mem_cb(env_cpu(env), addr, oi, QEMU_PLUGIN_MEM_R);
    return ret;
}

uint64_t cpu_ldq_le_mmu(CPUArchState *env, uint64_t addr,
                        MemOpIdx oi, uintptr_t ra)
{
    void *haddr;
    (void) haddr;
    uint64_t ret;

    validate_memop(oi, MO_LEUQ);
    haddr = cpu_mmu_lookup(env, addr, oi, ra, MMU_DATA_LOAD);
#if 0
    ret = ldq_le_p(haddr);
#else
    abort();
#endif
    clear_helper_retaddr();
    qemu_plugin_vcpu_mem_cb(env_cpu(env), addr, oi, QEMU_PLUGIN_MEM_R);
    return ret;
}

void cpu_stb_mmu(CPUArchState *env, uint64_t addr, uint8_t val,
                 MemOpIdx oi, uintptr_t ra)
{
    void *haddr;
    (void) haddr;

    validate_memop(oi, MO_UB);
    haddr = cpu_mmu_lookup(env, addr, oi, ra, MMU_DATA_STORE);
#if 0
    stb_p(haddr, val);
#else
    abort();
#endif
    clear_helper_retaddr();
    qemu_plugin_vcpu_mem_cb(env_cpu(env), addr, oi, QEMU_PLUGIN_MEM_W);
}

void cpu_stw_be_mmu(CPUArchState *env, uint64_t addr, uint16_t val,
                    MemOpIdx oi, uintptr_t ra)
{
    void *haddr;
    (void) haddr;

    validate_memop(oi, MO_BEUW);
    haddr = cpu_mmu_lookup(env, addr, oi, ra, MMU_DATA_STORE);
#if 0
    stw_be_p(haddr, val);
#else
    abort();
#endif
    clear_helper_retaddr();
    qemu_plugin_vcpu_mem_cb(env_cpu(env), addr, oi, QEMU_PLUGIN_MEM_W);
}

void cpu_stl_be_mmu(CPUArchState *env, uint64_t addr, uint32_t val,
                    MemOpIdx oi, uintptr_t ra)
{
    void *haddr;
    (void) haddr;

    validate_memop(oi, MO_BEUL);
    haddr = cpu_mmu_lookup(env, addr, oi, ra, MMU_DATA_STORE);
#if 0
    stl_be_p(haddr, val);
#else
    abort();
#endif
    clear_helper_retaddr();
    qemu_plugin_vcpu_mem_cb(env_cpu(env), addr, oi, QEMU_PLUGIN_MEM_W);
}

void cpu_stq_be_mmu(CPUArchState *env, uint64_t addr, uint64_t val,
                    MemOpIdx oi, uintptr_t ra)
{
    void *haddr;
    (void) haddr;

    validate_memop(oi, MO_BEUQ);
    haddr = cpu_mmu_lookup(env, addr, oi, ra, MMU_DATA_STORE);
#if 0
    stq_be_p(haddr, val);
#else
    abort();
#endif
    clear_helper_retaddr();
    qemu_plugin_vcpu_mem_cb(env_cpu(env), addr, oi, QEMU_PLUGIN_MEM_W);
}

void cpu_stw_le_mmu(CPUArchState *env, uint64_t addr, uint16_t val,
                    MemOpIdx oi, uintptr_t ra)
{
    void *haddr;
    (void) haddr;

    validate_memop(oi, MO_LEUW);
    haddr = cpu_mmu_lookup(env, addr, oi, ra, MMU_DATA_STORE);
#if 0
    stw_le_p(haddr, val);
#else
    abort();
#endif
    clear_helper_retaddr();
    qemu_plugin_vcpu_mem_cb(env_cpu(env), addr, oi, QEMU_PLUGIN_MEM_W);
}

void cpu_stl_le_mmu(CPUArchState *env, uint64_t addr, uint32_t val,
                    MemOpIdx oi, uintptr_t ra)
{
    void *haddr;
    (void) haddr;

    validate_memop(oi, MO_LEUL);
    haddr = cpu_mmu_lookup(env, addr, oi, ra, MMU_DATA_STORE);
#if 0
    stl_le_p(haddr, val);
#else
    abort();
#endif
    clear_helper_retaddr();
    qemu_plugin_vcpu_mem_cb(env_cpu(env), addr, oi, QEMU_PLUGIN_MEM_W);
}

void cpu_stq_le_mmu(CPUArchState *env, uint64_t addr, uint64_t val,
                    MemOpIdx oi, uintptr_t ra)
{
    void *haddr;
    (void) haddr;

    validate_memop(oi, MO_LEUQ);
    haddr = cpu_mmu_lookup(env, addr, oi, ra, MMU_DATA_STORE);
#if 0
    stq_le_p(haddr, val);
#else
    abort();
#endif
    clear_helper_retaddr();
    qemu_plugin_vcpu_mem_cb(env_cpu(env), addr, oi, QEMU_PLUGIN_MEM_W);
}

uint32_t cpu_ldub_code(CPUArchState *env, uint64_t ptr)
{
    uint32_t ret;

    set_helper_retaddr(1);
#if 0
    ret = ldub_p(g2h_untagged(ptr));
#else
    abort();
#endif
    clear_helper_retaddr();
    return ret;
}

uint32_t cpu_lduw_code(CPUArchState *env, uint64_t ptr)
{
    uint32_t ret;

    set_helper_retaddr(1);
#if 0
    ret = lduw_p(g2h_untagged(ptr));
#else
    abort();
#endif
    clear_helper_retaddr();
    return ret;
}

uint32_t cpu_ldl_code(CPUArchState *env, uint64_t ptr)
{
    uint32_t ret;

    set_helper_retaddr(1);
#if 0
    ret = ldl_p(g2h_untagged(ptr));
#else
    abort();
#endif
    clear_helper_retaddr();
    return ret;
}

uint64_t cpu_ldq_code(CPUArchState *env, uint64_t ptr)
{
    uint64_t ret;

    set_helper_retaddr(1);
#if 0
    ret = ldq_p(g2h_untagged(ptr));
#else
    abort();
#endif
    clear_helper_retaddr();
    return ret;
}

// #include "ldst_common.c.inc"

/*
 * Do not allow unaligned operations to proceed.  Return the host address.
 *
 * @prot may be PAGE_READ, PAGE_WRITE, or PAGE_READ|PAGE_WRITE.
 */
void *atomic_mmu_lookup(CPUArchState *env, uint64_t addr,
                        MemOpIdx oi, int size, int prot,
                        uintptr_t retaddr);
void *atomic_mmu_lookup(CPUArchState *env, uint64_t addr,
                        MemOpIdx oi, int size, int prot,
                        uintptr_t retaddr)
{
    MemOp mop = get_memop(oi);
    int a_bits = get_alignment_bits(mop);
    void *ret;

    /* Enforce guest required alignment.  */
    if (unlikely(addr & ((1 << a_bits) - 1))) {
        MMUAccessType t = prot == PAGE_READ ? MMU_DATA_LOAD : MMU_DATA_STORE;
        cpu_loop_exit_sigbus(env_cpu(env), addr, t, retaddr);
    }

    /* Enforce qemu required alignment.  */
    if (unlikely(addr & (size - 1))) {
        cpu_loop_exit_atomic(env_cpu(env), retaddr);
    }

#if 0
    ret = g2h(env_cpu(env), addr);
#else
    abort();
#endif
    set_helper_retaddr(retaddr);
    return ret;
}

#if 0
#include "atomic_common.c.inc"

/*
 * First set of functions passes in OI and RETADDR.
 * This makes them callable from other helpers.
 */

#define ATOMIC_NAME(X) \
    glue(glue(glue(cpu_atomic_ ## X, SUFFIX), END), _mmu)
#define ATOMIC_MMU_CLEANUP do { clear_helper_retaddr(); } while (0)

#define DATA_SIZE 1
#include "atomic_template.h"

#define DATA_SIZE 2
#include "atomic_template.h"

#define DATA_SIZE 4
#include "atomic_template.h"

#ifdef CONFIG_ATOMIC64
#define DATA_SIZE 8
#include "atomic_template.h"
#endif

#if HAVE_ATOMIC128 || HAVE_CMPXCHG128
#define DATA_SIZE 16
#include "atomic_template.h"
#endif
#endif
