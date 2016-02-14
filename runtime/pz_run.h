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

Imported_Proc builtin_print;
Imported_Proc builtin_int_to_string;
Imported_Proc builtin_free;

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
 * Update the offset to allow for any alignment required by the immediate
 * value.
 */
unsigned pz_immediate_alignment(Immediate_Type imm, unsigned last_offset);

/*
 * Get the in-memory size of the immediate value.
 */
unsigned pz_immediate_size(Immediate_Type imm);

/*
 * Return the size of the given instruction, excluding any immediate value.
 */
unsigned pz_instr_size(Opcode opcode);

/*
 * Write the instruction into the procedure at the given offset.
 */
void pz_write_instr(uint8_t *proc, unsigned offset, Opcode opcode,
    Operand_Width width1, Operand_Width width2);

/*
 * Write the immediate value (of various sizes) into the procedure at the
 * given offset.
 */
void
pz_write_imm8(uint8_t *proc, unsigned offset, uint8_t val);

void
pz_write_imm16(uint8_t *proc, unsigned offset, uint16_t val);

void
pz_write_imm32(uint8_t *proc, unsigned offset, uint32_t val);

void
pz_write_imm64(uint8_t *proc, unsigned offset, uint64_t val);

/*
 * TODO: Handle relative addressing and maybe PIC.
 */
void
pz_write_imm_word(uint8_t *proc, unsigned offset, uintptr_t val);

#endif /* ! PZ_RUN_H */
