/*
 * Plasma GC rooting utilities
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_GC_ROOTING_H
#define PZ_GC_ROOTING_H

#include <vector>

namespace pz {

class Tracer : public AbstractGCTracer {
  private:
    std::vector<void*> m_roots;

  public:
    Tracer() {}

    void add_root(void *root);

    /*
     * The roots must be removed in LIFO order.
     */
    void remove_root(void *root);

    Tracer(const Tracer&) = delete;
    Tracer& operator=(const Tracer&) = delete;
    
    virtual void do_trace(PZ_Heap_Mark_State *state) const;
};

template<typename T>
class Root {
  private:
    T      *m_gc_ptr;
    Tracer &m_tracer;

  public:
    Root(Tracer &t) : m_gc_ptr(nullptr), m_tracer(t)
    {
        m_tracer.add_root(&m_gc_ptr);
    }

    Root(const Root& r) :
        m_gc_ptr(r.gc_ptr),
        m_tracer(r.tracer)
    {
        m_tracer.add_root(&m_gc_ptr);
    }

    Root& operator=(const Root& r)
    {
        m_gc_ptr = r.gc_ptr;
    }

    ~Root()
    {
        m_tracer.remove_root(&m_gc_ptr);
    }

    const Root& operator=(T *ptr)
    {
        m_gc_ptr = ptr;
        return *this;
    }

    T* operator->() const
    {
        return m_gc_ptr;
    }

    T* get() const
    {
        return m_gc_ptr;
    }
};

} // namespace pz

#endif // ! PZ_GC_ROOTING_H
