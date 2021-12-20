/*
 * Plasma large memory region allocation
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2021 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>

#include "pz_memory.h"

bool
MemoryBase::allocate(size_t size) {
    void *ptr = mmap(NULL, size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

    if (MAP_FAILED == ptr) {
        return false;
    }
    m_pointer = ptr;
    m_size = size;
    return true;
}

bool
MemoryBase::release() {
    if (m_pointer) {
        if (-1 == munmap(m_pointer, m_size)) {
            perror("munmap");
            return false;
        }
        m_pointer = nullptr;
    }

    return true;
}

void
MemoryBase::forget() {
    m_pointer = nullptr;
}

