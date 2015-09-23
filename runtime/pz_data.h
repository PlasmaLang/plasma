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

#endif /* ! PZ_DATA_H */
