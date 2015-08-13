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
 */
pz_data* pz_data_init(uint_fast32_t num_data);

void pz_data_free(pz_data* data);

/*
 * Set the size of some data.  Sizes must be set in order as
 * internally they are represented as offsets from one-another.
 */
void pz_data_set_entry_size(pz_data* data, uint_fast32_t data_num,
    uint_fast32_t size);

#endif /* ! PZ_DATA_H */
