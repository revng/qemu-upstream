#include "helper2tcg/tcg_global_mappings.h"
#include "translate.h"

extern const char * new_value_names_ptr[TOTAL_PER_THREAD_REGS];
extern const char * reg_written_names_ptr[TOTAL_PER_THREAD_REGS];
extern const char * new_pred_value_names_ptr[NUM_PREGS];
extern const char * store_addr_names_ptr[STORES_MAX];
extern const char * store_width_names_ptr[STORES_MAX];
extern const char * store_val32_names_ptr[STORES_MAX];
extern const char * store_val64_names_ptr[STORES_MAX];
extern const char * const hexagon_prednames[];

cpu_tcg_mapping tcg_global_mappings[] = {
    /* General purpose and predicate registers */
    cpu_tcg_map_array(hex_gpr,  gpr,  hexagon_regnames),
    cpu_tcg_map_array(hex_pred, pred, hexagon_prednames),

    /* Misc */
    cpu_tcg_map(hex_new_value_usr,    new_value_usr,    "new_value_usr"),
    cpu_tcg_map(hex_slot_cancelled,   slot_cancelled,   "slot_cancelled"),
    cpu_tcg_map(hex_llsc_addr,        llsc_addr,        "llsc_addr"),
    cpu_tcg_map(hex_llsc_val,         llsc_val,         "llsc_val"),
    cpu_tcg_map(hex_llsc_val_i64,     llsc_val_i64,     "llsc_val_i64"),

    /*
     * New general purpose and predicate register values,
     * and reg_written used in debugging
     */
#if HEX_DEBUG
    cpu_tcg_map_array(hex_reg_written,    reg_written,    reg_written_names_ptr),
#endif

    /* Logging stores */
    cpu_tcg_map_array_of_structs(hex_store_addr,  mem_log_stores, va,     store_addr_names_ptr),
    cpu_tcg_map_array_of_structs(hex_store_width, mem_log_stores, width,  store_width_names_ptr),
    cpu_tcg_map_array_of_structs(hex_store_val32, mem_log_stores, data32, store_val32_names_ptr),
    cpu_tcg_map_array_of_structs(hex_store_val64, mem_log_stores, data64, store_val64_names_ptr),
};

size_t tcg_global_mapping_count = ARRAY_SIZE(tcg_global_mappings);
