/*
 * Plasma runtime options
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015, 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_OPTIONS_H
#define PZ_OPTIONS_H

#include <string>
#include <vector>

namespace pz {

/*
 * Runtime options
 *
 * Options are specified by environment variable, see README.md in this
 * directory for the list of configurable options.
 *
 * Not all options may be specified, some are compiled in as can be seen in
 * their accessor functions below.
 *
 * TODO: probably integrate options that can change at runtime with this
 * class, such as the GC size.
 */
class Options {
  public:
    enum Mode {
        NORMAL,
        HELP,
        VERSION,
        ERROR,
    };

  private:
    std::string                 m_pzfile;
    std::vector<std::string>    m_pzlibs;
    bool                        m_verbose;
    bool                        m_fast_exit;

#ifdef PZ_DEV
    bool        m_interp_trace;
    bool        m_gc_zealous;
    bool        m_gc_usage_stats;
    bool        m_gc_trace;
#endif

    // Non-null if parse returns Mode::ERROR
    const char *m_error_message;

    Mode parseCommandLine(int artc, char *const argv[]);
    void parseEnvironment();

  public:
    Options() : m_verbose(false)
#ifndef PZ_DEV
        , m_fast_exit(true)
#else
        , m_fast_exit(false)
        , m_interp_trace(false)
        , m_gc_zealous(false)
        , m_gc_usage_stats(false)
        , m_gc_trace(false)
#endif
    {}

    Mode parse(int artc, char *const argv[]);

    /*
     * Non-null if parse made an error message available.  Even if an error
     * occurs, sometimes getopt will print the error message and this will
     * be null.
     */
    const char * error_message() const { return m_error_message; }

    bool verbose() const { return m_verbose; }
    std::string pzfile() const { return m_pzfile; }
    const std::vector<std::string>& pzlibs() const { return m_pzlibs; }
    bool fast_exit() const { return m_fast_exit; }

#ifdef PZ_DEV
    bool interp_trace() const { return m_interp_trace; }
    bool gc_zealous() const { return m_gc_zealous; }
    bool gc_usage_stats() const { return m_gc_usage_stats; }

    // In the future make these false by default and allow them to be
    // changed at runtime.
    bool gc_slow_asserts() const { return true; }
    bool gc_poison() const { return true; }

    // Change temporarily to enable tracing.
    bool gc_trace() const { return m_gc_trace; }
    bool gc_trace2() const { return false; }
#else
    bool interp_trace() const { return false; }
#endif

    Options(const Options &) = delete;
    void operator=(const Options &) = delete;
};

}

#endif

