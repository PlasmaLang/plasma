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

#include <string>
#include <unordered_map>

#include "pz_closure.h"
#include "pz_code.h"
#include "pz_data.h"

#ifdef __cplusplus
extern "C" {
#endif

namespace pz {

class Module {
  private:
    std::vector<Struct>      structs;

    std::vector<void*>       datas;

    std::vector<Proc>        procs;
    unsigned                 total_code_size;

    std::vector<PZ_Closure*> closures;

    std::vector<PZ_Closure*> exports;
    unsigned                 next_export;

    std::unordered_map<std::string, unsigned>  symbols;
    Optional<unsigned>                         entry_closure_;

  public:
    Module();
    Module(unsigned num_structs,
           unsigned num_data,
           unsigned num_procs,
           unsigned num_closures,
           unsigned num_exports,
           int entry_closure);

    const Struct& struct_(unsigned id) const { return structs.at(id); }

    Struct& new_struct(unsigned num_fields);

    void * data(unsigned id) const { return datas.at(id); }

    void add_data(void *data);

    unsigned num_procs() const { return procs.size(); }

    Proc & proc(unsigned id) { return procs.at(id); }

    Proc & new_proc(PZ_Heap *heap, unsigned size);

    struct PZ_Closure_S * closure(unsigned id) const
    {
        return closures.at(id);
    }

    void set_closure(struct PZ_Closure_S *closure);

    Optional<unsigned> entry_closure() const { return entry_closure_; }

    void add_symbol(const std::string &name, struct PZ_Closure_S *closure);

    /*
     * Returns the ID of the closure in the exports struct.
     */
    Optional<unsigned> lookup_symbol(const std::string& name);

    struct PZ_Closure_S * export_(unsigned id) const { return exports.at(id); }

    void print_loaded_stats() const;

    void trace_for_gc(PZ_Heap_Mark_State *marker) const;

    Module(Module &other) = delete;
    void operator=(Module &other) = delete;
};

} // namespace pz

#ifdef __cplusplus
} // extern "C"
#endif

#endif // ! PZ_MODULE_H
