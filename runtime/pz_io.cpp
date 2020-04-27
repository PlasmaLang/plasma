/*
 * IO Utils.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015, 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include <stdio.h>
#include <stdlib.h>

#include "pz_common.h"

#include "pz_io.h"

namespace pz {

BinaryInput::~BinaryInput()
{
    if (m_file) {
        assert(!m_filename.empty());
        if (ferror(m_file)) {
            perror(m_filename.c_str());
        } else if (feof(m_file)) {
            fprintf(stderr, "%s: Unexpected end of file.\n", m_filename.c_str());
        }
        close();
    }
    assert(!m_file);
    assert(m_filename.empty());
}

bool
BinaryInput::open(const std::string &filename)
{
    assert(!m_file);
    assert(m_filename.empty());
    m_file = fopen(filename.c_str(), "rb");
    if (m_file) {
        m_filename = std::string(filename);
        return true;
    } else {
        return false;
    }
}

void
BinaryInput::close()
{
    assert(m_file);
    fclose(m_file);
    m_file = nullptr;
    assert(!m_filename.empty());
    m_filename.clear();
}

const std::string &
BinaryInput::filename() const
{
    return m_filename;
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
    return fseek(m_file, pos, SEEK_SET) == 0;
}

bool
BinaryInput::seek_cur(long pos)
{
    return fseek(m_file, pos, SEEK_CUR) == 0;
}

Optional<unsigned long>
BinaryInput::tell() const
{
    long pos = ftell(m_file);
    if (pos < 0) {
        return Optional<unsigned long>::Nothing();
    } else {
        return Optional<unsigned long>(pos);;
    }
}

bool
BinaryInput::is_at_eof()
{
    return !!feof(m_file);
}

bool
BinaryInput::read_uint8(uint8_t *value)
{
    return (1 == fread(value, sizeof(uint8_t), 1, m_file));
}

bool
BinaryInput::read_uint16(uint16_t *value)
{
    uint8_t bytes[2];

    if (!fread(bytes, sizeof(uint8_t), 2, m_file)) {
        return false;
    }

    *value = ((uint16_t)bytes[1] << 8) | (uint16_t)bytes[0];

    return true;
}

bool
BinaryInput::read_uint32(uint32_t *value)
{
    uint8_t bytes[4];

    if (!fread(bytes, sizeof(uint8_t), 4, m_file)) {
        return false;
    }

    *value = ((uint32_t)bytes[3] << 24) | ((uint32_t)bytes[2] << 16) |
             ((uint32_t)bytes[1] << 8) | (uint32_t)bytes[0];

    return true;
}

bool
BinaryInput::read_uint64(uint64_t *value)
{
    uint8_t bytes[8];

    if (!fread(bytes, sizeof(uint8_t), 8, m_file)) {
        return false;
    }

    *value = ((uint64_t)bytes[7] << 56) | ((uint64_t)bytes[6] << 48) |
             ((uint64_t)bytes[5] << 40) | ((uint64_t)bytes[4] << 32) |
             ((uint64_t)bytes[3] << 24) | ((uint64_t)bytes[2] << 16) |
             ((uint64_t)bytes[1] << 8) | (uint64_t)bytes[0];

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

const char *
BinaryInput::read_len_string(GCCapability &gc_cap)
{
    uint16_t len;

    if (!read_uint16(&len)) {
        return nullptr;
    }
    return read_string(gc_cap, len);
}

Optional<std::string>
BinaryInput::read_string(uint16_t len)
{
    char *buffer;

    buffer = (char*)malloc(sizeof(char) * (len + 1));
    if (len != fread(buffer, sizeof(char), len, m_file)) {
        free(buffer);
        return Optional<std::string>::Nothing();
    }
    buffer[len] = 0;
    // There's no way to build a C++ string without copying data.
    std::string string(buffer);
    free(buffer);

    return Optional<std::string>(string);
}

const char *
BinaryInput::read_string(GCCapability &gc_cap, uint16_t len)
{
    char *str;

    str = reinterpret_cast<char*>(gc_cap.alloc_bytes(
                sizeof(char) * (len + 1)));
    if (len != fread(str, sizeof(char), len, m_file)) {
        return nullptr;
    }
    str[len] = 0;

    return str;
}

}
