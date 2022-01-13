/*
 * Plasma large memory region allocation
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2021-2022 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>
#include <signal.h>
#include <cstring>

#include "pz_memory.h"

size_t MemoryBase::s_page_size = 0;
MemoryBase* MemoryBase::s_root = nullptr;

static void handler(int sig, siginfo_t *info, void *ucontext) {
    fprintf(stderr, "Caught signal ");
    switch (sig) {
        case SIGSEGV:
            fprintf(stderr, "SEGV");
            break;
        case SIGBUS:
            fprintf(stderr, "BUS");
            break;
        default:
            fprintf(stderr, "%d.\n", sig);
            return;
    }
    fprintf(stderr, " for address %p\n", info->si_addr);

    MemoryBase * zone = MemoryBase::search(info->si_addr);
    if (zone) {
        zone->fault_handler(info->si_addr);
    } else {
        fprintf(stderr,
                "The Plasma runtime doesn't know about this memory region.\n");
        exit(PZ_EXIT_RUNTIME_ERROR);
    }
}

void MemoryBase::fault_handler(void * fault_addr) {
    const char * juxt;
    InZone in = is_in(fault_addr);
    switch (in) {
      case IZ_WITHIN:
        juxt = "within";
        break;
      case IZ_GUARD_BEFORE:
        juxt = "in the guard page before";
        break;
      case IZ_GUARD_AFTER:
        juxt = "in the guard page after";
        break;
      case IZ_BEFORE:
      case IZ_AFTER:
        fprintf(stderr, "Fault is not in this zone (bad search result?)\n");
        abort();
    }
    fprintf(stderr, "The fault occured %s the %s region (%p - %p)\n",
            juxt, name(), first_address(), last_address());

    if (is_stack() && in == IZ_GUARD_AFTER) {
        fprintf(stderr, "This is probably caused by unbounded recursion causing "
                "a stack overrun\n");
    } else if (is_stack() && in == IZ_GUARD_BEFORE) {
        fprintf(stderr, "This could be a stack underrun, "
                "which is probably caused by a bug in the compiler");
    }

    exit(PZ_EXIT_RUNTIME_ERROR);
}

MemoryBase::InZone MemoryBase::is_in(void * fault_addr) const {
    assert(s_page_size);

    void * guard_before;
    void * last_addr_plus_1 = reinterpret_cast<void*>(
            reinterpret_cast<uintptr_t>(m_pointer) + m_size);
    void * guard_after;
    if (m_has_guards) {
        guard_before = reinterpret_cast<void*>(
                reinterpret_cast<uintptr_t>(m_pointer) - s_page_size);
        guard_after = reinterpret_cast<void*>(
                reinterpret_cast<uintptr_t>(last_addr_plus_1) + s_page_size);
    } else {
        guard_before = m_pointer;
        guard_after = last_addr_plus_1;
    }

    assert(guard_before <= m_pointer);
    assert(m_pointer < last_addr_plus_1);
    assert(last_addr_plus_1 <= guard_after);

    if (fault_addr < guard_before) {
        return IZ_BEFORE;
    } else if (fault_addr < m_pointer) {
        return IZ_GUARD_BEFORE;
    } else if (fault_addr < last_addr_plus_1) {
        return IZ_WITHIN;
    } else if (fault_addr < guard_after) {
        return IZ_GUARD_AFTER;
    } else {
        return IZ_AFTER;
    }
}

// Ignores errors, because they're not fatal.
static void setup_handler(int signal) {
    struct sigaction action;
    memset(&action, 0, sizeof(action));
    action.sa_sigaction = handler;
    sigemptyset(&action.sa_mask);
    sigaddset(&action.sa_mask, SIGSEGV);
    sigaddset(&action.sa_mask, SIGBUS);
    action.sa_flags = SA_SIGINFO;

    if (0 != sigaction(signal, &action, nullptr)) {
        perror("sigaction");
    }
}

void
MemoryBase::init_statics() {
    if (s_page_size) {
        // Init is already done.
        return;
    }

    s_page_size = sysconf(_SC_PAGESIZE);

    setup_handler(SIGSEGV);
    setup_handler(SIGBUS);
}

bool
MemoryBase::allocate(size_t size, bool guarded) {
    assert(s_page_size);

    size_t mmap_size = size;
    if (guarded) {
        mmap_size += s_page_size * 2;
    }

    void *ptr = mmap(NULL, mmap_size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

    if (MAP_FAILED == ptr) {
        return false;
    }

    if (guarded) {
        void * guard_address_1 = ptr;
        void * guard_address_2 = reinterpret_cast<void*>(
            reinterpret_cast<uintptr_t>(ptr) + s_page_size + size);
        ptr = reinterpret_cast<void*>(
                reinterpret_cast<uintptr_t>(ptr) + s_page_size);

        if (0 != mprotect(guard_address_1, s_page_size, PROT_NONE)) {
            return false;
        }
        if (0 != mprotect(guard_address_2, s_page_size, PROT_NONE)) {
            return false;
        }
    }

    m_pointer = ptr;
    m_size = size;
    m_has_guards = guarded;

    insert();

    return true;
}

bool
MemoryBase::release() {
    if (m_pointer) {
        remove();

        void *ptr = m_pointer;
        size_t size = m_size;
        if (m_has_guards) {
            ptr = reinterpret_cast<void*>(
                    reinterpret_cast<uintptr_t>(m_pointer) - s_page_size);
            size = m_size + s_page_size*2;
        }
        if (-1 == munmap(ptr, size)) {
            perror("munmap");
            return false;
        }
        m_pointer = nullptr;
    }

    return true;
}

void
MemoryBase::forget() {
    if (m_pointer) {
        remove();
        m_pointer = nullptr;
    }
}

MemoryBase*
MemoryBase::search(void * addr) {
    MemoryBase * cur = s_root;

    while (cur) {
        cur->check_node();

        switch (cur->is_in(addr)) {
          case IZ_BEFORE:
            cur = cur->m_left;
            continue;
          case IZ_AFTER:
            cur = cur->m_right;
            continue;
          case IZ_WITHIN:
          case IZ_GUARD_BEFORE:
          case IZ_GUARD_AFTER:
            return cur;
        }
    }

    return nullptr;
}

void
MemoryBase::insert() {
    MemoryBase **here = &s_root;
    MemoryBase *cur = s_root;
    MemoryBase *prev = nullptr;

    while (cur) {
        cur->check_node();

        switch (cur->is_in(this->m_pointer)) {
          case IZ_BEFORE:
            prev = cur;
            here = &cur->m_left;
            cur = cur->m_left;
            continue;
          case IZ_AFTER:
            prev = cur;
            here = &cur->m_right;
            cur = cur->m_right;
            continue;
          case IZ_WITHIN:
          case IZ_GUARD_BEFORE:
          case IZ_GUARD_AFTER:
            fprintf(stderr, "Duplicate map\n");
            abort();
        }
    }

    *here = this;
    m_parent = prev;
    check_node();
}

void
MemoryBase::remove() {
    check_node();

    MemoryBase **here;
    if (m_parent) {
        if (m_parent->m_left == this) {
            here = &m_parent->m_left;
        } else {
            here = &m_parent->m_right;
        }
    } else {
        here = &s_root;
    }

    if (!m_left && !m_right) {
        // A leaf node can be removed simply.
        *here = nullptr;
    } else if (!m_left && m_right) {
        // A node with only one child can be removed by replacing it with
        // its child.
        *here = m_right;
        m_right->m_parent = m_parent;
    } else if (m_left && !m_right) {
        // Ditto.
        *here = m_left;
        m_left->m_parent = m_parent;
    } else {
        // Find a node we can replace this node with.
        MemoryBase *cur = m_left;
        while (cur->m_right) {
            cur = cur->m_right;
        }

        // cur has no right branch, we can remove it from its position
        // easily.
        cur->remove();

        // Now replace this with cur.
        cur->m_parent = m_parent;
        *here = cur;
        cur->m_left = m_left;
        cur->m_right = m_right;

        // Fix the backlinks
        assert(cur != this); // We'd have entered one of te branches above
                             // if this were ==
        if (m_parent) {
            if (m_parent->m_left == this) {
                m_parent->m_left = cur;
            } else {
                assert(m_parent->m_right == this);
                m_parent->m_right = cur;
            }
        }
        assert(m_left->m_parent == this);
        m_left->m_parent = cur;
        assert(m_right->m_parent == this);
        m_right->m_parent = cur;

        cur->check_node();
    }

    m_parent = nullptr;
    m_left = nullptr;
    m_right = nullptr;
}

void MemoryBase::check_node() {
    if (!m_pointer) {
        assert(!m_left);
        assert(!m_right);
        assert(!m_parent);
        return;
    }

    // This is only called for a node in the tree, which means there is
    // always a non-null root node.
    assert(s_root);

    if (m_parent) {
        // check the relationship with our parent.
        assert(s_root != this);
        if (m_parent->m_left == this) {
            assert(m_parent->m_right != this);
            assert(m_pointer <= m_parent->m_pointer);
        } else {
            assert(m_parent->m_right == this);
            assert(m_parent->m_left != this);
            assert(m_parent->m_pointer <= m_pointer);
        }
    } else {
        assert(s_root == this);
    }

    if (m_left) {
        assert(m_left->m_parent == this);
        assert(m_left->m_pointer <= m_pointer);
    }
    if (m_right) {
        assert(m_right->m_parent == this);
        assert(m_pointer <= m_right->m_pointer);
    }
}

