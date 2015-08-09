/*
 * Plasma bytecode execution
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 *
 * This program executes plasma bytecode.
 */

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "pz.h"
#include "pz_read.h"


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
        pz *pz = read_pz(argv[optind], verbose);
        int retcode;
        retcode = run(pz);
        pz_free(pz);
        return retcode;
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

