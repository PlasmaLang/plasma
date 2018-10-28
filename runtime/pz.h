/*
 * Plasma in-memory representation
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016, 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_H
#define PZ_H

#include "pz_common.h"

#include "pz_module.h"

namespace pz {

/*
 * PZ Programs
 */
class PZ {
  private:
    RadixTree<Module*> *modules;
    Module             *entry_module_;

  public:
    PZ();
    ~PZ();

    /*
     * Add a module to the program.
     *
     * The entry module is not added in this way.
     *
     * The name will be strdup'd and so the caller is responsible for
     * freeing it after this call. The module will be freed by pz_free().
     */
    void add_module(const char *name, Module *module);

    Module * lookup_module(const char *name);

    void add_entry_module(Module *module);

    Module * entry_module() const { return entry_module_; }

    PZ(const PZ&) = delete;
    void operator=(const PZ&) = delete;
};

} // namespace pz

#endif /* ! PZ_H */
