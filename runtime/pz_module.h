/*
 * Plasma in-memory representation (modules)
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016, 2018-2019 Plasma Team
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
#include "pz_gc_util.h"

namespace pz {

class Export {
  private:
    Closure            *m_closure;
    Optional<unsigned>  m_export_id;

  public:
    explicit Export(Closure *closure);
    Export(Closure *closure, unsigned export_id);

    Closure* closure() const { return m_closure; }
    unsigned id() const { return m_export_id.value(); }
};

/*
 * This class tracks all the information we need to load a module, since
 * loading also includes linking.  Once that's complete a lot of this can be
 * dropped and only the exported symbols need to be kept (anything they
 * point to will be kept by the GC).
 */
class ModuleLoading : public AbstractGCTracer {
  private:
    std::vector<Struct*>     m_structs;

    std::vector<void*>       m_datas;

    std::vector<Proc*>       m_procs;
    unsigned                 m_total_code_size;

    std::vector<Closure*>    m_closures;

    unsigned                 m_next_export;

    std::unordered_map<std::string, Export> m_symbols;

    friend class Module;

  public:
    ModuleLoading(Heap *heap,
                  unsigned num_structs,
                  unsigned num_data,
                  unsigned num_procs,
                  unsigned num_closures);

    const Struct * struct_(unsigned id) const { return m_structs.at(id); }

    Struct * new_struct(unsigned num_fields, GCCapability &gc_cap);

    void * data(unsigned id) const { return m_datas.at(id); }

    void add_data(void *data);

    unsigned num_procs() const { return m_procs.size(); }

    const Proc * proc(unsigned id) const { return m_procs.at(id); }
    Proc * proc(unsigned id) { return m_procs.at(id); }

    Proc * new_proc(unsigned size, GCCapability &gc_cap);

    Closure * closure(unsigned id) const
    {
        return m_closures.at(id);
    }

    void set_closure(Closure *closure);

    void add_symbol(const std::string &name, Closure *closure);

    /*
     * Returns the ID of the closure in the exports struct.
     */
    Optional<unsigned> lookup_symbol(const std::string& name) const;

    void print_loaded_stats() const;

    ModuleLoading(ModuleLoading &other) = delete;
    void operator=(ModuleLoading &other) = delete;

    virtual void do_trace(HeapMarkState *marker) const;
};

class Module : public AbstractGCTracer {
  private:
    std::unordered_map<std::string, Export>     m_symbols;
    Closure                                    *m_entry_closure;

  public:
    Module(Heap *heap);
    Module(Heap *heap, ModuleLoading &loading, Closure *entry_closure);
    virtual ~Module() { };

    Closure * entry_closure() const { return m_entry_closure; }

    void add_symbol(const std::string &name, Closure *closure,
        unsigned export_id);

    Optional<Export> lookup_symbol(const std::string& name) const;

    virtual void do_trace(HeapMarkState *marker) const;

    Module(Module &other) = delete;
    void operator=(Module &other) = delete;
};

} // namespace pz

#endif // ! PZ_MODULE_H
