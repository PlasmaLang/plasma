/*
 * Plasma bytecode data and types loading and runtime
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 */

#ifndef PZ_DATA_H
#define PZ_DATA_H

struct pz_data {
    uint_fast32_t   num_datas;
    void**          data;
};

typedef struct pz_data pz_data;

/*
 * Create a new pz_data.
 */
pz_data* pz_data_init(uint_fast32_t num_data);

void pz_data_free(pz_data* data);

/*
 * Allocate space for basic data.  If the width is 0 then the data is a
 * reference to some other data, and should be machine word sized.
 */
void* pz_data_new_basic_data(unsigned raw_width);

/*
 * Allocate space for array data.  If the width is 0 then the array contains
 * references to other data, and each element should be machine word sized.
 */
void* pz_data_new_array_data(unsigned raw_width, uint32_t num_elements);

#endif /* ! PZ_DATA_H */
