/*
 * Plasma bytecode data and types loading and runtime
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_DATA_H
#define PZ_DATA_H

#include <vector>

#include "pz_cxx_future.h"
#include "pz_format.h"
#include "pz_gc.h"
#include "pz_gc_util.h"

namespace pz {

struct Struct_Field : public GCNew {
  private:
    PZ_Width     width;
    uint16_t     offset;

    Struct_Field() {};
    explicit Struct_Field(PZ_Width w) : width(w) {}

    friend class Struct;
};

class Struct : public GCNew {
  private:
    Struct_Field             *m_fields;
    unsigned                  m_num_fields;
    unsigned                  m_total_size;
#ifdef PZ_DEV
    bool                      m_layout_calculated;
#endif

  public:
    Struct() = delete;
    explicit Struct(NoGCScope &gc_cap, unsigned num_fields)
        : m_num_fields(num_fields)
#ifdef PZ_DEV
        , m_layout_calculated(false)
#endif
    {
        m_fields = new(gc_cap) Struct_Field[num_fields];
    }

    unsigned num_fields() const { return m_num_fields; }
    unsigned total_size() const { return m_total_size; }

    uint16_t field_offset(unsigned num) const
    {
#ifdef PZ_DEV
        assert(m_layout_calculated);
#endif
        assert(num < m_num_fields);
        return m_fields[num].offset;
    }

    void set_field(unsigned i, PZ_Width width)
    {
        m_fields[i] = Struct_Field(width);
    }

    void calculate_layout();

    Struct(const Struct &) = delete;
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
data_new_array_data(GCCapability &gc_tracer, PZ_Width width,
    uint32_t num_elements);

/*
 * Allocate space for struct data.
 */
void *
data_new_struct_data(GCCapability &gc_tracer, uintptr_t size);

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
