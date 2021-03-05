/*
 * Plasma bytecode execution
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2021 Plasma Team
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
#include "pz_util.h"

static int run(pz::Options & options);

static void help(const char * progname, FILE * stream);

static void version(void);

int main(int argc, char * const argv[])
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

static int run(pz::Options & options)
{
    using namespace pz;

    PZ pz(options);

    if (!pz.init()) {
        fprintf(stderr, "Couldn't initialise runtime.\n");
        return EXIT_FAILURE;
    }
    Delay finalise([&pz, &options] {
        if (!options.fast_exit()) {
            pz.finalise();
        }
    });

    Library * builtins = pz.new_library("Builtin");
    pz::setup_builtins(builtins, pz);

    for (auto & filename : options.pzlibs()) {
        Library * lib;
        std::vector<std::string> names;
        if (!read(pz, filename, &lib, names)) {
            return EXIT_FAILURE;
        }
        for (auto& name : names) {
            pz.add_library(name, lib);
        }
    }

    Library * program;
    std::vector<std::string> names; // XXX unused
    if (read(pz, options.pzfile(), &program, names)) {
        int retcode;

        pz.add_program_lib(program);
        retcode = run(pz, options);

        return retcode;
    } else {
        return EXIT_FAILURE;
    }
}

static void help(const char * progname, FILE * stream)
{
    fprintf(stream, "Plasma runtime\n\n");
    fprintf(stream, "    Run plasma bytecode programs\n\n");
    fprintf(stream, "Usage:\n\n");
    fprintf(stream, "    %s [-v] (-l <PZ LIB>) <PZ FILE> <program args>\n",
            progname);
    fprintf(stream, "    %s -h\n", progname);
    fprintf(stream, "    %s -V\n\n", progname);
    fprintf(stream, "Options:\n\n");
    fprintf(stream, "    -h     Show the help message (this one).\n");
    fprintf(stream, "    -V     Show version information.\n");
    fprintf(stream, "    -v     Verbose bytecode loading.\n");
    fprintf(stream, "    -l     Dynamic link this bytecode library.\n\n");
}

static void version(void)
{
    printf("Plasma runtime version: dev\n");
    printf("https://plasmalang.org\n");
    printf("Copyright (C) 2015-2021 The Plasma Team\n");
    printf("Distributed under the MIT License\n");
}
