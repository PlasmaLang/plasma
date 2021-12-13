/*
 * Plasma in-memory representation (modules)
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016, 2018-2021 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_LIBRARY_H
#define PZ_LIBRARY_H

#include "pz_common.h"

#include <unordered_map>

#include "pz_closure.h"
#include "pz_code.h"
#include "pz_data.h"
#include "pz_gc_util.h"

namespace pz {

/*
 * This class tracks all the information we need to load a library, since
 * loading also includes linking.  Once that's complete a lot of this can be
 * dropped and only the exported symbols need to be kept (anything they
 * point to will be kept by the GC).
 */
class LibraryLoading : public GCNewTrace
{
   private:
    std::vector<Struct *> m_structs;

    std::vector<void *> m_datas;

    std::vector<Proc *> m_procs;
    unsigned            m_total_code_size;

    std::vector<Closure *> m_closures;

    std::unordered_map<String, Closure *> m_symbols;

    friend class Library;

   public:
    LibraryLoading(unsigned num_structs,
                   unsigned num_data,
                   unsigned num_procs,
                   unsigned num_closures,
                   NoGCScope &no_gc);
    virtual ~LibraryLoading() { }

    const Struct * struct_(unsigned id) const
    {
        return m_structs.at(id);
    }

    Struct * new_struct(unsigned num_fields, GCCapability & gc_cap);

    void * data(unsigned id) const
    {
        return m_datas.at(id);
    }

    void add_data(void * data);

    unsigned num_procs() const
    {
        return m_procs.size();
    }

    const Proc * proc(unsigned id) const
    {
        return m_procs.at(id);
    }
    Proc * proc(unsigned id)
    {
        return m_procs.at(id);
    }

    Proc * new_proc(String name, unsigned size, bool is_builtin,
                    GCCapability & gc_cap);

    Closure * closure(unsigned id) const
    {
        return m_closures.at(id);
    }

    void add_symbol(String name, Closure * closure);

    void print_loaded_stats() const;

    LibraryLoading(LibraryLoading & other) = delete;
    void operator=(LibraryLoading & other) = delete;

    void do_trace(HeapMarkState * marker) const override;
};

class Library : public GCNewTrace
{
   private:
    std::unordered_map<String, Closure *>   m_symbols;
    PZOptEntrySignature                     m_entry_signature;
    Closure *                               m_entry_closure;

   public:
    Library();
    Library(LibraryLoading & loading);

    Closure * entry_closure() const
    {
        return m_entry_closure;
    }
    PZOptEntrySignature entry_signature() const
    {
        return m_entry_signature;
    }

    void set_entry_closure(PZOptEntrySignature sig, Closure * clo)
    {
        m_entry_signature = sig;
        m_entry_closure   = clo;
    }

    /*
     * Symbol names are fully qualified, since one Module class (which
     * really represents a library) may contain more than one modules.
     */
    void add_symbol(String name, Closure * closure);

    Optional<Closure *> lookup_symbol(String name) const;

    void do_trace(HeapMarkState * marker) const override;

    Library(Library & other) = delete;
    void operator=(Library & other) = delete;
};

}  // namespace pz

#endif  // ! PZ_LIBRARY_H
