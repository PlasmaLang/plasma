/*
 * IO Utils.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include <stdio.h>
#include <stdlib.h>

#include "pz_common.h"

#include "pz_io.h"

namespace pz {

BinaryInput::~BinaryInput()
{
    if (file_) {
        assert(!filename_.empty());
        if (ferror(file_)) {
            perror(filename_.c_str());
        } else if (feof(file_)) {
            fprintf(stderr, "%s: Unexpected end of file.\n", filename_.c_str());
        }
        close();
    }
    assert(!file_);
    assert(filename_.empty());
}

bool
BinaryInput::open(const char *filename)
{
    assert(!file_);
    assert(filename_.empty());
    file_ = fopen(filename, "rb");
    if (file_) {
        filename_ = std::string(filename);
        return true;
    } else {
        return false;
    }
}

void
BinaryInput::close()
{
    assert(file_);
    fclose(file_);
    file_ = nullptr;
    assert(!filename_.empty());
    filename_.clear();
}

const std::string &
BinaryInput::filename() const
{
    return filename_;
}

const char *
BinaryInput::filename_c() const
{
    return filename().c_str();
}

bool
BinaryInput::seek_set(long pos)
{
    assert(pos >= 0);
    return fseek(file_, pos, SEEK_SET) == 0;
}

bool
BinaryInput::seek_cur(long pos)
{
    return fseek(file_, pos, SEEK_CUR) == 0;
}

Optional<unsigned long>
BinaryInput::tell() const
{
    long pos = ftell(file_);
    if (pos < 0) {
        return Optional<unsigned long>::Nothing();
    } else {
        return Optional<unsigned long>(pos);;
    }
}

bool
BinaryInput::is_at_eof()
{
    return !!feof(file_);
}

bool
BinaryInput::read_uint8(uint8_t *value)
{
    return (1 == fread(value, sizeof(uint8_t), 1, file_));
}

bool
BinaryInput::read_uint16(uint16_t *value)
{
    uint8_t bytes[2];

    if (!fread(bytes, sizeof(uint8_t), 2, file_)) {
        return false;
    }

    *value = ((uint16_t)bytes[0] << 8) | (uint16_t)bytes[1];

    return true;
}

bool
BinaryInput::read_uint32(uint32_t *value)
{
    uint8_t bytes[4];

    if (!fread(bytes, sizeof(uint8_t), 4, file_)) {
        return false;
    }

    *value = ((uint32_t)bytes[0] << 24) | ((uint32_t)bytes[1] << 16) |
             ((uint32_t)bytes[2] << 8) | (uint32_t)bytes[3];

    return true;
}

bool
BinaryInput::read_uint64(uint64_t *value)
{
    uint8_t bytes[8];

    if (!fread(bytes, sizeof(uint8_t), 8, file_)) {
        return false;
    }

    *value = ((uint64_t)bytes[0] << 56) | ((uint64_t)bytes[1] << 48) |
             ((uint64_t)bytes[2] << 40) | ((uint64_t)bytes[3] << 32) |
             ((uint64_t)bytes[4] << 24) | ((uint64_t)bytes[5] << 16) |
             ((uint64_t)bytes[6] << 8) | (uint64_t)bytes[7];

    return true;
}

Optional<std::string>
BinaryInput::read_len_string()
{
    uint16_t len;

    if (!read_uint16(&len)) {
        return Optional<std::string>::Nothing();
    }
    return read_string(len);
}

Optional<std::string>
BinaryInput::read_string(uint16_t len)
{
    char *buffer;

    buffer = (char*)malloc(sizeof(char) * (len + 1));
    if (len != fread(buffer, sizeof(char), len, file_)) {
        free(buffer);
        return Optional<std::string>::Nothing();
    }
    buffer[len] = 0;
    // There's no way to build a C++ string without copying data.
    std::string string(buffer);
    free(buffer);

    return Optional<std::string>(string);
}

}
