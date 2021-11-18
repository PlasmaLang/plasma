/*
 * Plasma foreign code linker
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2021 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include <string>
#include <memory>
#include <unordered_map>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dlfcn.h>

#include "pz_cxx_future.h"
#include "pz_string.h"
#include "pz_gc_util.h"

#include "pz_foreign.h"

#define PZ_INIT_FOREIGN_CODE "pz_init_foreign_code"

namespace pz {

Foreign::Foreign(void * handle, foreign_library_cxx_function init_fn)
    : m_handle(handle)
    , m_init_fn(init_fn)
{}


static std::string safe_getcwd() {
    size_t len = 64;
    std::unique_ptr<char[]> buffer(new char[len]);
    char * result = getcwd(buffer.get(), len);

    // Try again with a larger buffer
    while (!result && errno == ERANGE) {
        len *= 2;
        buffer = std::unique_ptr<char[]>(new char[len]);
        result = getcwd(buffer.get(), len);
    }

    if (!result) {
        perror("getcwd()");
        exit(1);
    }

    return std::string(result);
}

Foreign::~Foreign() {
    if (m_handle) {
        dlclose(m_handle);
    }
}

// static
bool
Foreign::maybe_load(const std::string & filename, GCTracer &gc,
         Root<Foreign> &foreign)
{
    // Check that the library file exists, we need to do this ourselves
    // because dlload won't tell us.
    struct stat statbuf;
    if (-1 == stat(filename.c_str(), &statbuf)) {
        if (errno == ENOENT) {
            // The file doesn't exist
            return false;
        }
        // Some other error,
        perror(filename.c_str());
    }

    // The file probably exists, construct a path.
    std::string path;
    if (filename.length() > 0 && filename[0] == '/') {
        path = filename;
    } else {
        path = safe_getcwd() + "/" + filename;
    }

    // XXX: Use lazy resolution in release builds.
    void * handle = dlopen(path.c_str(), RTLD_NOW | RTLD_LOCAL);
    if (!handle) {
        fprintf(stderr, "%s\n", dlerror());
        return false;
    }

    dlerror(); // Clear the error state.
    foreign_library_cxx_function init_fn =
        reinterpret_cast<foreign_library_cxx_function>(
                dlsym(handle, PZ_INIT_FOREIGN_CODE));
    if (!init_fn) {
        const char * error = dlerror();
        if (error) {
            fprintf(stderr, "%s\n", error);
        } else {
            fprintf(stderr, "%s: Initial function is null\n",
                    filename.c_str());
        }
        return false;
    }

    foreign = new(gc) Foreign(handle, init_fn);
    return true;
}

bool
Foreign::init(GCTracer & gc) {
    assert(m_init_fn);
    return m_init_fn(this, &gc);
}

Closure *
Foreign::lookup_foreign_proc(String module_name, String closure_name) const {
    auto module = m_closures.find(module_name);
    if (module == m_closures.end()) {
        return nullptr;
    }
    auto closure = module->second.find(closure_name);
    if (closure == module->second.end()) {
        return nullptr;
    }
    return closure->second;
}

static unsigned make_ccall_instr(uint8_t * bytecode, pz_foreign_c_func c_func)
{
    ImmediateValue immediate_value;
    unsigned       offset = 0;

    immediate_value.word = (uintptr_t)c_func;
    offset +=
        write_instr(bytecode, offset, PZI_CCALL, IMT_PROC_REF, immediate_value);
    offset += write_instr(bytecode, offset, PZI_RET);

    return offset;
}

static void make_foreign(String name, pz_foreign_c_func c_func, GCTracer & gc,
        Foreign *foreign, Root<Closure> &closure)
{
    unsigned size = make_ccall_instr(nullptr, nullptr);

    Root<Proc> proc(gc);
    {
        NoGCScope nogc(gc);
        proc = new (nogc) Proc(nogc, name, true, size);
        nogc.abort_if_oom("setting up foreign code");
    }
    make_ccall_instr(proc->code(), c_func);

    // Use foreign as the closure's unused data pointer to ensure that the
    // Foreign object is referenced while closures may still point to its
    // code.
    closure = new (gc) Closure(proc->code(), foreign);
}

void
Foreign::do_trace(HeapMarkState * marker) const {
    for (auto m : m_closures) {
        marker->mark_root(m.first.ptr());
        for (auto c : m.second) {
            marker->mark_root(c.first.ptr());
            marker->mark_root(c.second);
        }
    }
}

bool
Foreign::register_foreign_code(String module, String proc,
        pz_foreign_c_func c_func, GCTracer & gc)
{
    Root<Closure> closure(gc);
    make_foreign(proc, c_func, gc, this, closure);

    m_closures[module][proc] = closure.ptr();

    return true;
}

}  // namespace pz

