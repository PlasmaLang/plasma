/*
 * Plasma bytecode execution
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 *
 * This program executes plasma bytecode.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "pz_builtin.h"
#include "pz_common.h"
#include "pz.h"
#include "pz_radix_tree.h"
#include "pz_read.h"
#include "pz_run.h"


static void help(const char *progname, FILE *stream);


int main(int argc, char * const argv[])
{
    bool verbose = false;
    int option;

    option = getopt(argc, argv, "vh");
    while (option != -1) {
        switch (option) {
            case 'h':
                help(argv[0], stdout);
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
        PZ_RadixTree    *builtin_symbols;
        PZ              *pz;

        builtin_symbols = pz_setup_builtins();
        pz = pz_read(argv[optind], verbose, builtin_symbols);
        pz_builtins_free(builtin_symbols);
        if (pz != NULL) {
            int retcode;
            retcode = pz_run(pz);
            pz_free(pz);
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


void help(const char *progname, FILE *stream)
{
    fprintf(stream, "%s [-v] <PZ FILE>\n", progname);
    fprintf(stream, "%s -h\n", progname);
}

