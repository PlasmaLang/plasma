/*
 * Plasma large memory region allocation
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2021 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_MEMORY_H
#define PZ_MEMORY_H

/*
 * A memory region, the address of the region is the pointer to Memory
 * itself,
 */
class Memory {
  private:
    void * m_pointer = nullptr;
    size_t m_size = 0;

  public:
    Memory() {}
    ~Memory() {
        release();
    }

    bool allocate(size_t size);
    void release();

    void * raw_pointer() const {
        return m_pointer;
    }

    bool is_mapped() const {
        return !!m_pointer;
    }

    Memory(Memory && other) = delete;
    Memory(const Memory & other) = delete;
    void operator=(Memory && other) = delete;
    void operator=(const Memory & other) = delete;
};

#endif /* ! PZ_MEMORY_H */

