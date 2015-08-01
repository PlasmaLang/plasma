/*
 * TLV Reader.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 */

#ifndef TLV_H
#define TLV_H

/*
 * When an operation fails it will either return NULL or boolean false.  In
 * these cases if errno != 0 then there was an IO error otherwise the end of
 * file was reached.
 */

/*
 * Read a tag and length from the stream.
 */
bool tlv_read_tag_length(FILE* stream, uint16_t* tag, uint16_t* length);

/*
 * Read an 8bit unsigned integer.
 */
bool tlv_read_uint8(FILE *stream, uint8_t *value);

/*
 * Read a 16bit unsigned integer.
 */
bool tlv_read_uint16(FILE* stream, uint16_t* value);

/*
 * Read a string of the given length from the stream.  If a string is
 * returned it is allocated on the heap and the caller takes responsability
 * for freeing it.
 */
char* tlv_read_string(FILE* stream, int16_t len);



#endif /* ! TLV_H */
