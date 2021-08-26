/*
 * IO Utils.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015, 2018-2019, 2021 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef IO_UTILS_H
#define IO_UTILS_H

#include <string>

#include "pz_cxx_future.h"
#include "pz_gc_util.h"
#include "pz_string.h"

namespace pz {

/*
 * A binary input file, this is a wrapper around a FILE pointer.  Internally
 * we use the C API rather than C++ since the C one is simple to use for
 * binary data.
 *
 * Since it wraps the C FILE structure a failing operation will set errno.
 * Callers should check errno directly.
 */
class BinaryInput
{
   private:
    FILE *      m_file;
    std::string m_filename;

   public:
    BinaryInput() : m_file(nullptr), m_filename() {}

    /*
     * For normal/happy paths, you must call close() before the destructor
     * runs.  The destructor will treat the file being open as an error and
     * report information about the file's state.
     */
    ~BinaryInput();

    /*
     * Open a file.
     */
    bool open(const std::string & filename);

    /*
     * Close the file.
     */
    void close();

    /*
     * The current file's name.
     */
    const std::string & filename() const;
    const char *        filename_c() const;

    /*
     * Read an 8bit unsigned integer.
     */
    bool read_uint8(uint8_t * value);

    /*
     * Read a 16bit unsigned integer.
     */
    bool read_uint16(uint16_t * value);

    /*
     * Read a 32bit unsigned integer.
     */
    bool read_uint32(uint32_t * value);

    /*
     * Read a 64bit unsigned integer.
     */
    bool read_uint64(uint64_t * value);

    /*
     * Read a length (16 bits) followed by a string of that length.
     */
    Optional<String>      read_len_string(GCCapability & gc_cap);

    /*
     * Read a string of the given length from the stream.
     */
    Optional<String>      read_string(GCCapability & gc_cap, uint16_t len);

    /*
     * seek relative to beginning of file.
     */
    bool seek_set(long pos);

    /*
     * seek relative to current position.
     */
    bool seek_cur(long pos);

    Optional<unsigned long> tell() const;

    bool is_at_eof();

    BinaryInput(const BinaryInput &) = delete;
    void operator=(const BinaryInput &) = delete;
};

}  // namespace pz

#endif /* ! IO_UTILS_H */
