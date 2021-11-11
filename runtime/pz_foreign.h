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

namespace pz {

class Foreign;

typedef bool (*foreign_library_cxx_function)(Foreign & foreign);

class Foreign {
  private:
    void * m_handle;
    foreign_library_cxx_function m_init_fn;

    Foreign(void * handle, foreign_library_cxx_function init_fn);

  public:
    ~Foreign();

    static Optional<Foreign> maybe_load(const std::string & filename);
    bool init();

    // Not copyable since it has unique resource ownership.
    Foreign(const Foreign &) = delete;
    const Foreign & operator=(const Foreign &) = delete;

    Foreign(Foreign && other); 

    const Foreign & operator=(Foreign && other);
};

}  // namespace pz

#endif /* ! PZ_FOREIGN_H */
