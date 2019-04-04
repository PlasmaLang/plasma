/*
 * Plasma closures
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_GENERIC_CLOSURE_H
#define PZ_GENERIC_CLOSURE_H

#include "pz_gc.h"

namespace pz {

class Closure {
  private:
    void     *m_code;
    void     *m_data;

  public:
    Closure(void *code, void *data) :
        m_code(code), m_data(data) {};

    void* code() const { return m_code; }
    void* data() const { return m_data; }
};

Closure *
new_closure(Heap *heap, GCCapability &gc_cap, uint8_t *code, void *data);

}

#endif // !PZ_GENERIC_CLOSURE_H
