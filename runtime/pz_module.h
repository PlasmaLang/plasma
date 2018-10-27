/*
 * Plasma in-memory representation (modules)
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016, 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_MODULE_H
#define PZ_MODULE_H

#include "pz_common.h"

#include <string.h>

#include "pz_closure.h"
#include "pz_code.h"
#include "pz_data.h"
#include "pz_radix_tree.h"

#ifdef __cplusplus
extern "C" {
#endif

namespace pz {

class Module {
  private:
    std::vector<Struct> structs;

    unsigned      num_datas;
    void        **datas;

    Proc        **procs;
    unsigned      num_procs;
    unsigned      total_code_size;

    PZ_Closure  **closures;
    unsigned      num_closures;

    PZ_Closure  **exports;
    unsigned      num_exports;
    unsigned      next_export;
    RadixTree<unsigned> *symbols;

    int32_t       entry_closure_;

  public:
    Module(unsigned num_structs,
           unsigned num_data,
           unsigned num_procs,
           unsigned num_closures,
           unsigned num_exports,
           int entry_closure);
    ~Module();

    const Struct& struct_(unsigned id) const { return structs.at(id); }

    Struct& new_struct(unsigned num_fields);

    void * data(unsigned id) const { return datas[id]; }

    void set_data(unsigned id, void *data_)
    {
        datas[id] = data_;
    }

    Proc * proc(unsigned id) { return procs[id]; }

    void set_proc(unsigned id, Proc *proc)
    {
        assert(NULL == procs[id]);
        procs[id] = proc;
        total_code_size += proc->size();
    }

    struct PZ_Closure_S * closure(unsigned id) const
    {
        return closures[id];
    }

    void set_closure(unsigned id, struct PZ_Closure_S *closure)
    {
        closures[id] = closure;
    }

    int32_t entry_closure() const { return entry_closure_; }

    void add_symbol(const char *name, struct PZ_Closure_S *closure);

    /*
     * Returns the ID of the closure in the exports struct.  -1 if not found.
     */
    int lookup_symbol(const char *name);

    struct PZ_Closure_S * export_(unsigned id) const { return exports[id]; }

    void print_loaded_stats() const;

    Module() = delete;
    Module(Module &other) = delete;
    void operator=(Module &other) = delete;
};

} // namespace pz

#ifdef __cplusplus
} // extern "C"
#endif

#endif // ! PZ_MODULE_H
