/*
 * Plasma bytecode exection
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_RUN_H
#define PZ_RUN_H

#include "pz_instructions.h"
#include "pz_format.h"
#include "pz.h"

/*
 * Imported procedures
 *
 **********************/

typedef enum {
    BUILTIN,
    BUILTIN_FOREIGN
} Import_Type;

typedef struct {
    Import_Type type;
    void        *proc;
} Imported_Proc;

extern Imported_Proc builtin_print;
extern Imported_Proc builtin_int_to_string;
extern Imported_Proc builtin_free;
extern Imported_Proc builtin_concat_string;
extern Imported_Proc builtin_die;

/*
 * The size of "fast" integers in bytes.
 */
extern unsigned pz_fast_word_size;


/*
 * Run the program.
 *
 ******************/

int pz_run(PZ *pz);

/*
 * Build the raw code of the program.
 *
 ************************************/

/*
 * Write the instruction into the procedure at the given offset.
 * Returns the new offset within the procedure for the next instruction.
 * If proc is NULL then nothing is written but a new offset is computed,
 * this can be used in a first pass to calculate the required size of the
 * procedure.
 */
unsigned pz_write_instr(uint8_t *proc, unsigned offset, Opcode opcode,
    Width width1, Width width2, Immediate_Type imm_type, Immediate_Value imm);

unsigned pz_width_to_bytes(Width width);

#endif /* ! PZ_RUN_H */
