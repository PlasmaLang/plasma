/*
 * Plasma runtime options
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015, 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_OPTIONS_H
#define PZ_OPTIONS_H

#include <string>

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
    std::string pzfile_;
    bool        verbose_;

#ifdef PZ_DEV
    bool        interp_trace_;
    bool        gc_zealous_;
#endif

    // Non-null if parse returns Mode::ERROR
    const char *error_message_;

    Mode parseCommandLine(int artc, char *const argv[]);
    void parseEnvironment();

  public:
    Options() : verbose_(false)
#ifdef PZ_DEV
        , interp_trace_(false)
        , gc_zealous_(false)
#endif
    {}

    Mode parse(int artc, char *const argv[]);

    /*
     * Non-null if parse made an error message available.  Even if an error
     * occurs, sometimes getopt will print the error message and this will
     * be null.
     */
    const char * error_message() const { return error_message_; }

    bool verbose() const { return verbose_; }
    std::string pzfile() const { return pzfile_; }

#ifdef PZ_DEV
    bool interp_trace() const { return interp_trace_; }
    bool gc_zealous() const { return gc_zealous_; }

    // Set this to false by default and allow to be adjusted in the future.
    bool gc_slow_asserts() const { return true; }

    // Change temporarilly to enable tracing.
    bool gc_trace() const { return false; }
    bool gc_trace2() const { return false; }
#endif
};

}

#endif

