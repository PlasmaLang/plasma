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
    uint_fast32_t   *data_offsets;
    uint_fast32_t   total_size;
    uint8_t         *data;
};

typedef struct pz_data pz_data;

/*
 * Create a new pz_data.
 * The array of data offsets is num_data items long, it will be deallocated
 * when pz_data_free is called.
 */
pz_data* pz_data_init(uint_fast32_t num_data, uint_fast32_t* data_offsets,
    uint_fast32_t total_size);

void pz_data_free(pz_data* data);

#endif /* ! PZ_DATA_H */
