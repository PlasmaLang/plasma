/*
 * Plasma bytecode execution
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include <string.h>
#include <unistd.h>

// ??
#include <stdlib.h>

#include "pz_option.h"

namespace pz {

Options::Mode
Options::parse(int argc, char *const argv[])
{
    m_error_message = nullptr;
    Mode mode = parseCommandLine(argc, argv);

    if (mode == Mode::ERROR) return Mode::ERROR;

    parseEnvironment();

    return mode;
}

#ifdef _GNU_SOURCE
// Request POSIX behaviour
#define OPTSTRING "+hl:vV"
#else
#define OPTSTRING "hl:vV"
#endif

Options::Mode
Options::parseCommandLine(int argc, char *const argv[])
{
    int option = getopt(argc, argv, OPTSTRING);
    while (option != -1) {
        switch (option) {
            case 'h':
                return Mode::HELP;
            case 'l':
                m_pzlibs.emplace_back(optarg);
                break;
            case 'V':
                return Mode::VERSION;
            case 'v':
                m_verbose = true;
                break;
            case '?':
                return Mode::ERROR;
        }
        option = getopt(argc, argv, OPTSTRING);
    }

    if (optind < argc) {
        m_pzfile = argv[optind];
    } else {
        m_error_message = "Expected one PZB file to execute";
        return Mode::ERROR;
    }

    return Mode::NORMAL;
}

void
Options::parseEnvironment()
{
    if (char *opts = getenv("PZ_RUNTIME_OPTS")) {
        opts = strdup(opts);
        char *strtok_save;

        const char *token = strtok_r(opts, ",", &strtok_save);
        while (token) {
            if (strcmp(token, "load_verbose") == 0) {
                m_verbose = true;
            } else if (strncmp(token, "fast_exit", 9) == 0) {
                if (token[9] == '=') {
                    if (strcmp(&token[10], "yes") == 0) {
                        m_fast_exit = true;
                    } else if (strcmp(&token[10], "no") == 0) {
                        m_fast_exit = false;
                    } else {
                        fprintf(stderr,
                            "PZ_RUNTIME_OPTS option fast_exit bad parameter "
                            "'%s', expected 'yes' or 'no'.\n",
                            &token[10]);
                    }
                } else {
                    fprintf(stderr,
                        "PZ_RUNTIME_OPTS "
                        "option fast_exit requires a parameter\n");
                }
            } else {
                // This warning is non-fatal, so it doesn't set the
                // error_message_ property or return ERROR.
                fprintf(stderr,
                        "Warning: Unknown PZ_RUNTIME_OPTS option: %s\n",
                        token);
            }
            token = strtok_r(nullptr, ",", &strtok_save);
        }

        free(opts);
    }

#ifdef PZ_DEV
    if (char *opts = getenv("PZ_RUNTIME_DEV_OPTS")) {
        opts = strdup(opts);
        char *strtok_save;

        const char *token = strtok_r(opts, ",", &strtok_save);
        while (token) {
            if (strcmp(token, "interp_trace") == 0) {
                m_interp_trace = true;
            } else if (strcmp(token, "gc_zealous") == 0) {
                m_gc_zealous = true;
            } else if (strcmp(token, "gc_usage_stats") == 0) {
                m_gc_usage_stats = true;
            } else if (strcmp(token, "gc_trace") == 0) {
                m_gc_trace = true;
            } else {
                // This warning is non-fatal, so it doesn't set the
                // error_message_ property or return ERROR.
                fprintf(stderr,
                        "Warning: Unknown PZ_RUNTIME_DEV_OPTS option: %s\n",
                        token);
            }
            token = strtok_r(nullptr, ",", &strtok_save);
        }

        free(opts);
    }
#endif
}

}

