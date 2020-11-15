/*
 * PZ C++ future library functions,.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include "pz_cxx_future.h"

bool startsWith(const std::string & string, const char * beginning)
{
    auto iter = string.begin();
    while (*beginning) {
        if (iter > string.end()) {
            return false;
        }
        if (*beginning != *iter) {
            return false;
        }
        beginning++;
        iter++;
    }

    return true;
}
