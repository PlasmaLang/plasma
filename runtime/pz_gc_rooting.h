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

/*
 * GCTracer helps the GC find the roots, it traces in order to find the
 * GC roots.
 */
class GCTracer : public AbstractGCTracer {
  private:
    std::vector<void*> m_roots;

    /*
     * This code is currently unused, but it might be useful in the future.
     * Disable it until then so that if/when it gets reused we can review
     * it.
     *
     * Specifically:
     *
     * It could be dangerous to create tracers easilly because doing so
     * might allow the developer to place a /can gc/ scope inside a /no gc/
     * scope.
     */
    GCTracer();

  public:
    void add_root(void *root);

    /*
     * The roots must be removed in LIFO order.
     */
    void remove_root(void *root);

    GCTracer(const GCTracer&) = delete;
    GCTracer& operator=(const GCTracer&) = delete;

    virtual void do_trace(HeapMarkState *state) const;
};

template<typename T>
class Root {
  private:
    T        *m_gc_ptr;
    GCTracer &m_tracer;

  public:
    Root(GCTracer &t) : m_gc_ptr(nullptr), m_tracer(t)
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
