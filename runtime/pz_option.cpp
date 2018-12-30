/*
 * Plasma bytecode execution
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2018 Plasma Team
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
    error_message_ = nullptr;
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
                verbose_ = true;
                break;
            case '?':
                return Mode::ERROR;
        }
        option = getopt(argc, argv, "vh");
    }

    if (optind + 1 == argc) {
        pzfile_ = argv[optind];
    } else {
        error_message_ = "Expected exactly one PZ file to execute";
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
                verbose_ = true;
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
}

}

