/*
 * Plasma bytecode code structures and functions
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016, 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_CODE_H
#define PZ_CODE_H

#include "pz_gc.h"
#include "pz_gc_rooting.h"

namespace pz {

/*
 * Code layout in memory
 *
 *************************/
class Proc {
  private:
    uint8_t     *m_code;
    unsigned     m_code_size;

  public:
    Proc(Heap *heap, Traceable &traceable, unsigned size);

    uint8_t * code() const { return m_code; }
    unsigned size() const { return m_code_size; }

    Proc() = delete;

    // TODO: I'd like to to restrict this, but right now vector<Proc>
    // requires it.
    // Proc(const Proc&) = delete;
    void operator=(const Proc &other) = delete;
};

} // namespace pz

#endif /* ! PZ_CODE_H */
