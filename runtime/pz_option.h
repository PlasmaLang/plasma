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
#endif
};

}

#endif

