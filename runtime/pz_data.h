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

namespace pz {

struct Struct_Field {
    PZ_Width width;
    uint16_t offset;
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
    Struct(unsigned num_fields) : fields(num_fields)
#ifdef PZ_DEV
                                , layout_calculated(false)
#endif
                                {}

    unsigned num_fields() const { return fields.size(); }
    unsigned total_size() const { return total_size_; }

    uint16_t field_offset(unsigned num) const
    {
#ifdef PZ_DEV
        assert(layout_calculated);
#endif
        return fields.at(num).offset;
    }

    void set_field_width(unsigned num, PZ_Width width)
    {
        fields.at(num).width = width;
    }

    void calculate_layout();

    void operator=(const Struct &other) = delete;
};

} // namespace pz

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Data
 *
 *******/

/*
 * Allocate space for array data.  If the width is 0 then the array contains
 * references to other data, and each element should be machine word sized.
 */
void *
pz_data_new_array_data(unsigned raw_width, uint32_t num_elements);

/*
 * Allocate space for struct data.
 */
void *
pz_data_new_struct_data(uintptr_t size);

/*
 * Free any of the above data entries.
 */
void
pz_data_free(void *data);

/*
 * Functions for storing data in memory
 ***************************************/

/*
 * Write the given value into the data object.
 */
void
pz_data_write_normal_uint8(void *dest, uint8_t value);
void
pz_data_write_normal_uint16(void *dest, uint16_t value);
void
pz_data_write_normal_uint32(void *dest, uint32_t value);
void
pz_data_write_normal_uint64(void *dest, uint64_t value);

/*
 * Write the given value into the data object.  The value will be sign
 * extended to the "fast" width.
 */
void
pz_data_write_fast_from_int32(void *dest, int32_t value);

void
pz_data_write_wptr(void *dest, intptr_t value);

/*
 * When given the fast width, return the equivalent absolute width.
 */
PZ_Width
pz_normalize_width(PZ_Width w);

unsigned
pz_width_to_bytes(PZ_Width width);

#ifdef __cplusplus
}
#endif

#endif /* ! PZ_DATA_H */
