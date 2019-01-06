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

/*
 * This class tracks all the information we need to load a module, since
 * loading also includes linking.  Once that's complete a lot of this can be
 * dropped and only the exported symbols need to be kept (anything they
 * point to will be kept by the GC).
 */
class ModuleLoading {
  private:
    std::vector<Struct>      structs;

    std::vector<void*>       datas;

    std::vector<Proc>        procs;
    unsigned                 total_code_size;

    std::vector<PZ_Closure*> closures;

    std::vector<PZ_Closure*> exports;
    unsigned                 next_export;

    std::unordered_map<std::string, unsigned>  symbols;

    friend class Module;

  public:
    ModuleLoading();
    ModuleLoading(unsigned num_structs,
                  unsigned num_data,
                  unsigned num_procs,
                  unsigned num_closures,
                  unsigned num_exports);

    const Struct& struct_(unsigned id) const { return structs.at(id); }

    Struct& new_struct(unsigned num_fields);

    void * data(unsigned id) const { return datas.at(id); }

    void add_data(void *data);

    unsigned num_procs() const { return procs.size(); }

    Proc & proc(unsigned id) { return procs.at(id); }

    Proc & new_proc(Heap *heap, unsigned size);

    struct PZ_Closure_S * closure(unsigned id) const
    {
        return closures.at(id);
    }

    void set_closure(struct PZ_Closure_S *closure);

    void add_symbol(const std::string &name, struct PZ_Closure_S *closure);

    /*
     * Returns the ID of the closure in the exports struct.
     */
    Optional<unsigned> lookup_symbol(const std::string& name);

    struct PZ_Closure_S * export_(unsigned id) const { return exports.at(id); }

    void print_loaded_stats() const;

    // TODO we will need this in case we GC during loading.
    void trace_for_gc(PZ_Heap_Mark_State *marker) const;

    ModuleLoading(ModuleLoading &other) = delete;
    void operator=(ModuleLoading &other) = delete;
};

class Module {
  private:
    std::vector<PZ_Closure*>                    exports;
    std::unordered_map<std::string, unsigned>   symbols;
    PZ_Closure                                 *entry_closure_;

  public:
    Module();
    Module(ModuleLoading &loading, PZ_Closure *entry_closure);

    PZ_Closure * entry_closure() const { return entry_closure_; }

    void add_symbol(const std::string &name, struct PZ_Closure_S *closure);

    /*
     * Returns the ID of the closure in the exports struct.
     */
    Optional<unsigned> lookup_symbol(const std::string& name);

    struct PZ_Closure_S * export_(unsigned id) const { return exports.at(id); }

    void trace_for_gc(PZ_Heap_Mark_State *marker) const;

    Module(Module &other) = delete;
    void operator=(Module &other) = delete;
};

} // namespace pz

#ifdef __cplusplus
} // extern "C"
#endif

#endif // ! PZ_MODULE_H
