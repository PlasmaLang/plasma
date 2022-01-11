/*
 * Plasma large memory region allocation
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2021-2022 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_MEMORY_H
#define PZ_MEMORY_H

#include <type_traits>

class MemoryBase {
  // Static stuff:
  private:
    static size_t s_page_size;

  public:
    static void init_statics();

  // Per memory-mapping stuff:
  private:
    void * m_pointer = nullptr;
    size_t m_size = 0;
    bool   m_has_guards = false;

    // Memory mappings are arranged in a tree for signal handlers to find
    // them.
    MemoryBase * m_left = nullptr;
    MemoryBase * m_right = nullptr;
    MemoryBase * m_parent = nullptr;
    static MemoryBase * s_root;

    static MemoryBase * search(void * pointer);
    void insert();
    void remove();
    void check_node();

  public:
    MemoryBase() {}
    ~MemoryBase() {
        release();
    }

    bool is_mapped() const {
        return !!m_pointer;
    }

  protected:
    bool allocate(size_t size, bool guard);

    void * raw_pointer() const {
        return m_pointer;
    }

  public:
    // Release the memory back to the OS
    bool release();

    // Forget the memory mapping, much faster, very leaky.
    void forget();

    MemoryBase(MemoryBase && other) = delete;
    MemoryBase(const MemoryBase & other) = delete;
    void operator=(MemoryBase && other) = delete;
    void operator=(const MemoryBase & other) = delete;

    enum InZone {
        IZ_BEFORE,
        IZ_AFTER,
        IZ_WITHIN,
        IZ_GUARD_BEFORE,
        IZ_GUARD_AFTER,
    };

    // Describe where this memory address lies compared with the mapped
    // memory region.
    InZone is_in(void * addr) const;

    static void fault_handler(void * fault_addr);

    void * first_address() const {
        return m_pointer;
    }

    void * last_address() const {
        if (m_pointer) {
            return reinterpret_cast<void*>(
                    reinterpret_cast<uintptr_t>(m_pointer) + m_size - 1);
        } else {
            return nullptr;
        }
    }
};

/*
 * A memory region, the address of the region is the pointer to Memory
 * itself,
 */
template<typename T>
class Memory : public MemoryBase {
  public:
    bool allocate(size_t size = sizeof(T)) {
        return MemoryBase::allocate(size, false);
    }

    // Allocate with guard pages before and after the allocation.
    bool allocate_guarded(size_t size = sizeof(T)) {
        return MemoryBase::allocate(size, true);
    }

    T * ptr() {
        return reinterpret_cast<T*>(raw_pointer());
    }
    const T * ptr() const {
        return reinterpret_cast<T*>(raw_pointer());
    }
    T * operator->() {
        return ptr();
    }
    const T * operator->() const {
        return ptr();
    }

    typedef typename std::remove_all_extents<T>::type Elem;
    Elem& operator[](unsigned i) {
        return reinterpret_cast<Elem*>(ptr())[i];
    }
};

#endif /* ! PZ_MEMORY_H */

