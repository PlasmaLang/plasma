/*
 * Plasma bytecode data and types loading and runtime
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_DATA_H
#define PZ_DATA_H

#include <vector>

#include "pz_format.h"
#include "pz_cxx_future.h"

namespace pz {

class Struct_Field {
  private:
    PZ_Width     width;
    uint16_t     offset;

    Struct_Field(PZ_Width w) : width(w) {}

    friend class Struct;
};

class Struct {
  private:
    std::vector<Struct_Field> fields;
    unsigned                  total_size_;
#ifdef PZ_DEV
    bool                      layout_calculated;
#endif

  public:
    Struct() = delete;
    Struct(unsigned num_fields) :
#ifdef PZ_DEV
                                  layout_calculated(false)
#endif
    {
        fields.reserve(num_fields);
    }

    unsigned num_fields() const { return fields.size(); }
    unsigned total_size() const { return total_size_; }

    uint16_t field_offset(unsigned num) const
    {
#ifdef PZ_DEV
        assert(layout_calculated);
#endif
        return fields.at(num).offset;
    }

    void add_field(PZ_Width width)
    {
        fields.push_back(Struct_Field(width));
    }

    void calculate_layout();

    void operator=(const Struct &other) = delete;
};

Optional<PZ_Width>
width_from_int(uint8_t w);

PZ_Width
width_normalize(PZ_Width w);

unsigned
width_to_bytes(PZ_Width w);

/*
 * Data
 *
 *******/

/*
 * Allocate space for array data.  If the width is 0 then the array contains
 * references to other data, and each element should be machine word sized.
 */
void *
data_new_array_data(PZ_Width width, uint32_t num_elements);

/*
 * Allocate space for struct data.
 */
void *
data_new_struct_data(uintptr_t size);

/*
 * Free any of the above data entries.
 */
void
data_free(void *data);

/*
 * Functions for storing data in memory
 ***************************************/

/*
 * Write the given value into the data object.
 */
void
data_write_normal_uint8(void *dest, uint8_t value);
void
data_write_normal_uint16(void *dest, uint16_t value);
void
data_write_normal_uint32(void *dest, uint32_t value);
void
data_write_normal_uint64(void *dest, uint64_t value);

/*
 * Write the given value into the data object.  The value will be sign
 * extended to the "fast" width.
 */
void
data_write_fast_from_int32(void *dest, int32_t value);

void
data_write_wptr(void *dest, intptr_t value);

} // namespace pz

#endif /* ! PZ_DATA_H */
