/*
 * Plasma bytecode data and types loading and runtime
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_DATA_H
#define PZ_DATA_H

#include "pz_format.h"

/*
 * Structs
 *
 **********/

typedef struct PZ_Structs_Struct {
    unsigned                    num_structs;
    struct PZ_Struct_Struct     *structs;
} PZ_Structs;

typedef struct PZ_Struct_Struct {
    unsigned            num_fields;
    Width               *field_widths;
} PZ_Struct;

/*
 * Create a new set of structs.  Initially the structs are undefined.
 */
PZ_Structs *pz_structs_init(unsigned num_structs);

/*
 * Free a set of structs.  This will free all the referenced structs
 */
void pz_structs_free(PZ_Structs *structs);

/*
 * Create a new struct with the given ID.  This places the struct in the set
 * of structs.  Returns the array of field widths for the struct which the
 * caller will then populate.
 */
Width* pz_new_struct(PZ_Structs *structs, unsigned struct_id,
    unsigned num_fields);

/*
 * Data
 *
 *******/

typedef struct PZ_Data_Struct {
    uint_fast32_t   num_datas;
    void            **data;
} PZ_Data;

/*
 * Create a new pz_data.
 */
PZ_Data *pz_data_init(uint_fast32_t num_data);

void pz_data_free(PZ_Data *data);

/*
 * Allocate space for basic data.  If the width is 0 then the data is a
 * reference to some other data, and should be machine word sized.
 */
void *pz_data_new_basic_data(unsigned raw_width);

/*
 * Allocate space for array data.  If the width is 0 then the array contains
 * references to other data, and each element should be machine word sized.
 */
void *pz_data_new_array_data(unsigned raw_width, uint32_t num_elements);

/*
 * Return a pointer to the given data entry.
 */
void *pz_data_get_data(PZ_Data *data, uint32_t id);

/*
 * Functions for storing data in memory
 ***************************************/

/*
 * Write the given value into the data object.
 */
void pz_data_write_normal_uint8(void *dest, uint8_t value);
void pz_data_write_normal_uint16(void *dest, uint16_t value);
void pz_data_write_normal_uint32(void *dest, uint32_t value);
void pz_data_write_normal_uint64(void *dest, uint64_t value);

/*
 * Write the given value into the data object.  The value will be sign
 * extended to the "fast" width.
 */
void pz_data_write_fast_from_int32(void *dest, int32_t value);

void pz_data_write_wptr(void *dest, intptr_t value);

#endif /* ! PZ_DATA_H */
