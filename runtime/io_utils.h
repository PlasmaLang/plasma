/*
 * IO Utils.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef IO_UTILS_H
#define IO_UTILS_H

/*
 * When an operation fails it will either return NULL or boolean false.  In
 * these cases if errno != 0 then there was an IO error otherwise the end of
 * file was reached.
 */

/*
 * Read an 8bit unsigned integer.
 */
bool read_uint8(FILE *stream, uint8_t *value);

/*
 * Read a 16bit unsigned integer.
 */
bool read_uint16(FILE *stream, uint16_t *value);

/*
 * Read a 32bit unsigned integer.
 */
bool read_uint32(FILE *stream, uint32_t *value);

/*
 * Read a 64bit unsigned integer.
 */
bool read_uint64(FILE *stream, uint64_t *value);

/*
 * Read a length (16 bits) followed by a string of that length.
 */
char* read_len_string(FILE *stream);

/*
 * Read a string of the given length from the stream.  If a string is
 * returned it is allocated on the heap and the caller takes responsibility
 * for freeing it.
 */
char* read_string(FILE *stream, int16_t len);

#endif /* ! IO_UTILS_H */
