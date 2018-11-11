/*
 * Plasma bytecode code structures and functions
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016, 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_CODE_H
#define PZ_CODE_H

#include "pz_gc.h"

namespace pz {

/*
 * Code layout in memory
 *
 *************************/
class Proc {
  private:
    uint8_t     *code_;
    unsigned     code_size;

  public:
    Proc(unsigned size) : code_(new uint8_t[size]), code_size(size) {}
    ~Proc()
    {
        delete[] code_;
    }

    uint8_t * code() const { return code_; }
    unsigned size() const { return code_size; }

    Proc() = delete;
    void operator=(const Proc &other) = delete;
};

} // namespace pz

#endif /* ! PZ_CODE_H */
