/*
 * Plasma GC rooting, scopes & C++ allocation utilities
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018-2021 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_GC_UTIL_H
#define PZ_GC_UTIL_H

#include <vector>

#include "pz_gc.h"

namespace pz {

// Forward declarations.
class AbstractGCTracer;

/*
 * This is the base class that the GC will use to determine if its legal to
 * GC.  Do not create subclasses of this, use only AbstractGCTracer.
 */
class GCCapability
{
   private:
    Heap               &m_heap;
#ifdef PZ_DEV
    GCCapability       *m_parent;
#else
    const GCCapability *m_parent;
#endif
    const bool          m_can_gc;
#ifdef PZ_DEV
    bool                m_is_top = true;
#endif

   protected:
    GCCapability(Heap & heap, bool can_gc)
        : m_heap(heap)
        , m_parent(nullptr)
        , m_can_gc(can_gc) {}
    // TODO: Check heirachy.
    GCCapability(GCCapability & gc_cap, bool can_gc)
        : m_heap(gc_cap.heap())
        , m_parent(&gc_cap)
        , m_can_gc(can_gc)
    {
#ifdef PZ_DEV
        gc_cap.m_is_top = false;
#endif
    }

#ifdef PZ_DEV
    ~GCCapability() {
        assert(m_is_top);
        if (m_parent) {
            assert(!m_parent->m_is_top);
            m_parent->m_is_top = true;
        }
    }
#endif

   public:
    void * alloc(size_t size_in_words, AllocOpts opts = AllocOpts::NORMAL);
    void * alloc_bytes(size_t    size_in_bytes,
                       AllocOpts opts = AllocOpts::NORMAL);

    Heap & heap() const {
        return m_heap;
    }

    bool can_gc() const;

    // Called by the GC if we couldn't allocate this much memory.
    virtual void oom(size_t size_bytes) = 0;

    /*
     * This casts to AbstractGCTracer whenever can_gc() returns true, so
     * AbstractGCTracer must be the only subclass that overrides can_gc() to
     * return true.
     */
    const AbstractGCTracer & tracer() const;
};

// Each thread gets one of these.  Do not create more than one per thread.
class GCThreadHandle : public GCCapability {
  public:
    GCThreadHandle(Heap & heap) : GCCapability(heap, true) {}

    virtual void oom(size_t size_bytes);
};

/*
 * AbstractGCTracer helps the GC find the roots, it traces in order to find
 * the GC roots.
 *
 * Roots are traced from two different sources (both use this class).
 * Global roots and thread-local roots.
 */
class AbstractGCTracer : public GCCapability
{
   public:
    AbstractGCTracer(GCCapability & gc) : GCCapability(gc, true) {}

    virtual void oom(size_t size);
    virtual void do_trace(HeapMarkState *) const = 0;

   private:
    /*
     * A work-around for PZ
     */
    AbstractGCTracer() = default;
    AbstractGCTracer(Heap & heap) : GCCapability(heap, true) { }
    friend class PZ;
};

class NoRootsTracer : public AbstractGCTracer
{
   public:
    NoRootsTracer(GCCapability & gc_cap) : AbstractGCTracer(gc_cap) {}

    virtual void do_trace(HeapMarkState *) const {};
};

/*
 * GCTracer helps the GC find the roots, it traces in order to find the
 * GC roots.
 */
class GCTracer : public AbstractGCTracer
{
   private:
    std::vector<void *> m_roots;

   public:
    GCTracer(GCCapability & gc_cap) : AbstractGCTracer(gc_cap) {}

    void add_root(void * root);

    /*
     * The roots must be removed in LIFO order.
     */
    void remove_root(void * root);

    GCTracer(const GCTracer &) = delete;
    GCTracer & operator=(const GCTracer &) = delete;

    virtual void do_trace(HeapMarkState * state) const;
};

template <typename T>
class Root
{
   private:
    T *        m_gc_ptr;
    GCTracer & m_tracer;

   public:
    explicit Root(GCTracer & t) : m_gc_ptr(nullptr), m_tracer(t)
    {
        m_tracer.add_root(&m_gc_ptr);
    }

    explicit Root(GCTracer & t, T * ptr) : m_gc_ptr(ptr), m_tracer(t)
    {
        m_tracer.add_root(&m_gc_ptr);
    }

    Root(const Root & r) : m_gc_ptr(r.m_gc_ptr), m_tracer(r.m_tracer)
    {
        m_tracer.add_root(&m_gc_ptr);
    }

    const Root & operator=(const Root & r)
    {
        m_gc_ptr = r.gc_ptr;
    }

    ~Root()
    {
        m_tracer.remove_root(&m_gc_ptr);
    }

    const Root & operator=(T * ptr)
    {
        m_gc_ptr = ptr;
        return *this;
    }

    T * operator->() const
    {
        return m_gc_ptr;
    }

    const T * ptr() const {
        return m_gc_ptr;
    }
    T * ptr() {
        return m_gc_ptr;
    }

    const T & get() const {
        return *m_gc_ptr;
    }
    T & get() {
        return *m_gc_ptr;
    }
};

/*
 * Use this RAII class to create scopes where GC is forbidden.
 *
 * Needing to GC (due to memory pressure) is handled by returning nullptr
 * (normally allocation is infalliable).  This class will return nullptr and
 * the require the caller to check either is_oom() or abort_if_oom() before
 * the end of the NoGCScope.  You can allocate a series of things and
 * perform the check at the end of the scope.
 *
 * This is not C++ conformant.  We'd need to use the C++ new handler or
 * exceptions or nothrow forms to do that.  We could be tempting fate but it
 * seems that it's okay either to throw or use -fno-exceptions.  See:
 * https://blog.mozilla.org/nnethercote/2011/01/18/the-dangers-of-fno-exceptions/
 */
class NoGCScope : public GCCapability
{
   private:
#ifdef PZ_DEV
    bool m_needs_check;
#endif

    bool   m_did_oom;
    size_t m_oom_size;

   public:
    // The constructor may use the tracer to perform an immediate
    // collection, or if it is a NoGCScope allow the direct nesting.
    NoGCScope(GCCapability & gc_cap);
    virtual ~NoGCScope();

    virtual bool can_gc() const
    {
        return false;
    }
    virtual void oom(size_t size);

    // Assert if there was an OOM.  This is available for inlining because
    // we don't want to leave the fast-path unless the test fails.
    void abort_if_oom(const char * label)
    {
        if (m_did_oom) {
            abort_for_oom_slow(label);
        }
#if PZ_DEV
        // If there are further allocations this won't be reset before the
        // destructor runs.  This isn't fool-proof.
        m_needs_check = false;
#endif
    }

    bool is_oom()
    {
#if PZ_DEV
        m_needs_check = false;
#endif
        return m_did_oom;
    }

   protected:
    void abort_for_oom_slow(const char * label);
};

class GCNew
{
   public:
    /*
     * Operator new is infalliable, it'll abort the program if the
     * GC returns null, which it can only do in a NoGCScope.
     */
    void * operator new(size_t size, GCCapability & gc_cap);
    // We don't need a placement-delete or regular-delete because we use GC.
};

/*
 * A GC allocatable object with tracing and a finaliser.  This is necessary
 * if the class uses the regular heap (eg via STL collections).
 */
class GCNewTrace : public GCNew
{
   public:
    virtual ~GCNewTrace(){};
    virtual void do_trace(HeapMarkState * marker) const = 0;

    void * operator new(size_t size, GCCapability & gc_cap);
};

}  // namespace pz

// Array allocation for any type.  Intended for arrays of primative types
// like integers, floats and pointers.
void * operator new[](size_t size, pz::GCCapability & gc_cap);

#endif  // ! PZ_GC_UTIL_H
