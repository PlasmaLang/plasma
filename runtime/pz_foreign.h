/*
 * Plasma foreign code linker
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2021 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_FOREIGN_H
#define PZ_FOREIGN_H

#include <string>

#include "pz_cxx_future.h"
#include "pz_interp.h"

namespace pz {

class Foreign;

typedef bool (*foreign_library_cxx_function)(Foreign * foreign, GCTracer * gc);

class Foreign {
  private:
    void * m_handle;
    foreign_library_cxx_function m_init_fn;
    std::unordered_map<String, std::unordered_map<String, Closure*>>
        m_closures =
        std::unordered_map<String, std::unordered_map<String, Closure*>>(1);

    Foreign(void * handle, foreign_library_cxx_function init_fn);

  public:
    ~Foreign();

    static Optional<Foreign> maybe_load(const std::string & filename);
    bool init(GCTracer & gc);

    // Not copyable since it has unique resource ownership.
    Foreign(const Foreign &) = delete;
    const Foreign & operator=(const Foreign &) = delete;

    Foreign(Foreign && other); 

    const Foreign & operator=(Foreign && other);

    Closure * lookup_foreign_proc(String module, String proc) const;

    /*
     * These functions help setup foreign code.
     */
    bool register_foreign_code(String module, String proc, 
            pz_builtin_c_func c_func, GCTracer & gc);
};

}  // namespace pz

#endif /* ! PZ_FOREIGN_H */
