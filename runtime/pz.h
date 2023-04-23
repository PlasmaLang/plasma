/*
 * Plasma in-memory representation
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_H
#define PZ_H

#include "pz_common.h"

#include <memory>
#include <string>
#include <unordered_map>

#include "pz_library.h"

namespace pz {
/*
 * PZ Programs
 */
class PZ : public AbstractGCTracer
{
   private:
    const Options &                            m_options;
    std::unordered_map<String, Library *>      m_libraries;
    Library *                                  m_program;

   public:
    explicit PZ(const Options & options, Heap & heap);
    ~PZ();

    Library * new_library(const String name, GCCapability & gc_cap);

    const Options & options() const
    {
        return m_options;
    }

    /*
     * Add a library to the program.
     *
     * The main program library (it is a Library class) is not added in this
     * way.
     *
     * The name will be coppied and the caller remains responsible for
     * the original name. The module will be freed by pz_free().
     */
    void add_library(const String name, Library * library);

    Library * lookup_library(const String name);

    void add_program_lib(Library * module);

    Library * program_lib() const
    {
        return m_program;
    }

    PZ(const PZ &) = delete;
    void operator=(const PZ &) = delete;

    void do_trace(HeapMarkState * marker) const override;
};

}  // namespace pz

#endif /* ! PZ_H */
