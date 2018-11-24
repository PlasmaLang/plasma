/*
 * Plasma bytecode execution
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 *
 * This program executes plasma bytecode.
 */

#include <stdio.h>
#include <unistd.h>

#include "pz_common.h"

#include "pz.h"
#include "pz_builtin.h"
#include "pz_interp.h"
#include "pz_read.h"

static void
help(const char *progname, FILE *stream);

static void
version(void);

int
main(int argc, char *const argv[])
{
    using namespace pz;

    bool verbose = false;
    int  option;

    option = getopt(argc, argv, "vVh");
    while (option != -1) {
        switch (option) {
            case 'h':
                help(argv[0], stdout);
                return EXIT_SUCCESS;
            case 'V':
                version();
                return EXIT_SUCCESS;
            case 'v':
                verbose = true;
                break;
            case '?':
                help(argv[0], stderr);
                return EXIT_FAILURE;
        }
        option = getopt(argc, argv, "vh");
    }
    if (optind + 1 == argc) {
        Module *builtins;
        Module *module;
        PZ      pz;

        if (!pz.init()) {
            fprintf(stderr, "Couldn't initialise runtime.\n");
            return EXIT_FAILURE;
        }

        builtins = pz::setup_builtins();
        assert(builtins != nullptr);
        pz.add_module("builtin", builtins);
        module = read(pz, std::string(argv[optind]), verbose);
        if (module != NULL) {
            int retcode;

            pz.add_entry_module(module);
            retcode = run(pz);

            return retcode;
        } else {
            return EXIT_FAILURE;
        }
    } else {
        fprintf(stderr, "Expected exactly one PZ file\n");
        help(argv[0], stderr);
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

static void
help(const char *progname, FILE *stream)
{
    fprintf(stream, "%s [-v] <PZ FILE>\n", progname);
    fprintf(stream, "%s -h\n", progname);
    fprintf(stream, "%s -V\n", progname);
}

static void
version(void)
{
    printf("Plasma runtime version: dev\n");
    printf("https://plasmalang.org\n");
    printf("Copyright (C) 2015-2018 The Plasma Team\n");
    printf("Distributed under the MIT License\n");
}
