/*
 * Plasma bytecode data and types loading and runtime
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 */

#ifndef PZ_DATA_H
#define PZ_DATA_H

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
