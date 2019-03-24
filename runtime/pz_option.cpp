/*
 * Plasma bytecode execution
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include <string.h>
#include <string>
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

Options::Mode
Options::parseCommandLine(int argc, char *const argv[])
{
    int option = getopt(argc, argv, "vVh");
    while (option != -1) {
        switch (option) {
            case 'h':
                return Mode::HELP;
            case 'V':
                return Mode::VERSION;
            case 'v':
                m_verbose = true;
                break;
            case '?':
                return Mode::ERROR;
        }
        option = getopt(argc, argv, "vh");
    }

    if (optind + 1 == argc) {
        m_pzfile = argv[optind];
    } else {
        m_error_message = "Expected exactly one PZ file to execute";
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

