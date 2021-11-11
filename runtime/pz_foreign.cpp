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
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dlfcn.h>

#include "pz_cxx_future.h"
#include "pz_foreign.h"

#define PZ_INIT_FOREIGN_CODE "pz_init_foreign_code"

namespace pz {

Foreign::Foreign(void * handle, foreign_library_cxx_function init_fn)
    : m_handle(handle)
    , m_init_fn(init_fn) 
{}


Foreign::Foreign(Foreign && other) 
    : m_handle(other.m_handle)
    , m_init_fn(other.m_init_fn) 
{
    other.m_handle = nullptr;
}

const Foreign &
Foreign::operator=(Foreign && other) {
    assert(!m_handle);

    m_handle = other.m_handle;
    m_init_fn = other.m_init_fn;

    other.m_handle = nullptr;

    return *this;
}

static std::string replace_extension(const std::string & filename,
        const char * extension)
{
    size_t pos = filename.find_last_of('.');
    std::string base_name;
    if (pos == std::string::npos) {
        // There is no extension to remove
        base_name = filename;
    } else {
        base_name = filename.substr(0, pos);
    }

    return std::move(base_name) + extension;
}

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
Optional<Foreign>
Foreign::maybe_load(const std::string & filename_) {
    std::string filename = replace_extension(filename_, ".so");

    // Check that the library file exists, we need to do this ourselves
    // because dlload won't tell us.
    struct stat statbuf;
    if (-1 == stat(filename.c_str(), &statbuf)) {
        if (errno == ENOENT) {
            // The file doesn't exist, return no foreign code library
            return Optional<Foreign>();
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
        return Optional<Foreign>();
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
        return Optional<Foreign>();
    }

    return Optional<Foreign>(Foreign(handle, init_fn));
}

bool
Foreign::init() {
    assert(m_init_fn);
    return m_init_fn(*this);
}

}  // namespace pz

