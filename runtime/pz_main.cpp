/*
 * Plasma bytecode execution
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 *
 * This program executes plasma bytecode.
 */

#include <stdio.h>

#include "pz_common.h"

#include "pz.h"
#include "pz_builtin.h"
#include "pz_interp.h"
#include "pz_option.h"
#include "pz_read.h"

static int
run(pz::Options &options);

static void
help(const char *progname, FILE *stream);

static void
version(void);

int
main(int argc, char *const argv[])
{
    using namespace pz;

    Options options;

    Options::Mode mode = options.parse(argc, argv);
    switch (mode) {
        case Options::Mode::HELP:
            help(argv[0], stdout);
            return EXIT_SUCCESS;
        case Options::Mode::VERSION:
            version();
            return EXIT_SUCCESS;
        case Options::Mode::ERROR:
            if (options.error_message()) {
                fprintf(stderr, "%s: %s\n", argv[0], options.error_message());
            }
            help(argv[0], stderr);
            return EXIT_FAILURE;
        case Options::Mode::NORMAL:
            return run(options);
    }
}

static int
run(pz::Options &options)
{
    using namespace pz;

    Module *module;
    PZ      pz(options);

    if (!pz.init()) {
        fprintf(stderr, "Couldn't initialise runtime.\n");
        return EXIT_FAILURE;
    }

    Module *builtins = pz.new_module("builtin");
    pz::setup_builtins(builtins);
    module = read(pz, options.pzfile());
    if (module != nullptr) {
        int retcode;

        pz.add_entry_module(module);
        retcode = run(pz, options);
        pz.finalise();

        return retcode;
    } else {
        pz.finalise();
        return EXIT_FAILURE;
    }
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
    printf("Copyright (C) 2015-2019 The Plasma Team\n");
    printf("Distributed under the MIT License\n");
}
