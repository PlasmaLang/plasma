/*
 * IO Utils.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef IO_UTILS_H
#define IO_UTILS_H

#include <string>

#include "pz_util.h"

namespace pz {

/*
 * A binary input file, this is a wrapper around a FILE pointer.  Internally
 * we use the C API rather than C++ since the C one is simple to use for
 * binary data.
 *
 * Since it wraps the C FILE structure a failing operation will set errno.
 * Callers should check errno directly.
 */
class BinaryInput {
  private:
    FILE        *file_;
    std::string  filename_;

  public:
    BinaryInput() :
        file_(nullptr),
        filename_() {}

    /*
     * For normal/happy paths, you must call close() before the destructor
     * runs.  The destructor will treat the file being open as an error and
     * report information about the file's state.
     */
    ~BinaryInput();

    /*
     * Open a file.
     */
    bool open(const char *filename);

    /*
     * Close the file.
     */
    void close();

    /*
     * Read an 8bit unsigned integer.
     */
    bool read_uint8(uint8_t *value);

    /*
     * Read a 16bit unsigned integer.
     */
    bool read_uint16(uint16_t *value);

    /*
     * Read a 32bit unsigned integer.
     */
    bool read_uint32(uint32_t *value);

    /*
     * Read a 64bit unsigned integer.
     */
    bool read_uint64(uint64_t *value);

    /*
     * Read a length (16 bits) followed by a string of that length.
     */
    char * read_len_string();

    /*
     * Read a string of the given length from the stream.  If a string is
     * returned it is allocated on the heap and the caller takes
     * responsibility for freeing it.
     */
    char * read_string(uint16_t len);

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

    BinaryInput(const BinaryInput&) = delete;
    void operator=(const BinaryInput&) = delete;
};

} // namespace pz

#endif /* ! IO_UTILS_H */
