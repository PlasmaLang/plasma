/*
 * Plasma in-memory representation
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016, 2018-2020 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_H
#define PZ_H

#include "pz_common.h"

#include <memory>
#include <string>
#include <unordered_map>

#include "pz_gc.h"

#include "pz_library.h"

namespace pz {
/*
 * PZ Programs
 */
class PZ : public AbstractGCTracer
{
   private:
    const Options & m_options;
    std::unordered_map<std::string, Library *> m_libraries;
    Library * m_program;
    std::unique_ptr<Heap> m_heap;

   public:
    explicit PZ(const Options & options);
    ~PZ();

    bool init();
    bool finalise();

    Heap * heap()
    {
        return m_heap.get();
    }

    Library * new_library(const std::string & name);

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
    void add_library(const std::string & name, Library * library);

    Library * lookup_library(const std::string & name);

    void add_program_lib(Library * module);

    Library * program_lib() const
    {
        return m_program;
    }

    PZ(const PZ &) = delete;
    void operator=(const PZ &) = delete;

    virtual void do_trace(HeapMarkState * marker) const;
};

}  // namespace pz

#endif /* ! PZ_H */
