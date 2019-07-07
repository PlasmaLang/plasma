/*
 * Plasma in-memory representation
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016, 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_H
#define PZ_H

#include "pz_common.h"

#include <memory>
#include <string>
#include <unordered_map>

#include "pz_gc.h"

#include "pz_module.h"

namespace pz {

/*
 * PZ Programs
 */
class PZ : public AbstractGCTracer {
  private:
    const Options                            &m_options;
    std::unordered_map<std::string, Module*>  m_modules;
    std::unique_ptr<Module>                   m_entry_module;
    std::unique_ptr<Heap>                     m_heap;

  public:
    explicit PZ(const Options &options);
    ~PZ();

    bool init();
    bool finalise();

    Heap * heap() { return m_heap.get(); }

    Module * new_module(const std::string &name);

    /*
     * Add a module to the program.
     *
     * The entry module is not added in this way.
     *
     * The name will be strdup'd and so the caller is responsible for
     * freeing it after this call. The module will be freed by pz_free().
     */
    void add_module(const std::string &name, Module *module);

    Module * lookup_module(const std::string &name);

    void add_entry_module(Module *module);

    Module * entry_module() const { return m_entry_module.get(); }

    PZ(const PZ&) = delete;
    void operator=(const PZ&) = delete;

    virtual void do_trace(HeapMarkState *marker) const;
};

} // namespace pz

#endif /* ! PZ_H */
