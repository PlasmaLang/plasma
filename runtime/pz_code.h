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

/*
 * Code layout in memory
 *
 *************************/

class PZ_Proc {
  private:
    uint8_t     *code_;
    unsigned     code_size;

  public:
    PZ_Proc() = delete;
    PZ_Proc(unsigned size) : code_(new uint8_t[size]), code_size(size) {}
    ~PZ_Proc()
    {
        delete[] code_;
    }

    void operator=(const PZ_Proc &other) = delete;

    uint8_t * code() const { return code_; }
    unsigned size() const { return code_size; }
};

#endif /* ! PZ_CODE_H */
