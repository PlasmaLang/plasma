/*
 * Plasma strings
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2021 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include <string.h>

#include "pz_common.h"

#include "pz_string.h"

namespace pz {

String*
String::append(GCCapability &gc, const String *s1, const String *s2) {
    uint32_t len = s1->length() + s2->length() + 1;
    FlatString *s = FlatString::New(gc, len);
    strcpy(reinterpret_cast<char*>(s->buffer()), s1->c_str());
    strcat(reinterpret_cast<char*>(s->buffer()), s2->c_str());
    return s;
}

bool
String::equals(const String &other) const {
    return 0 == strcmp(c_str(), other.c_str());
}

/*
 * FlatString
 *************/

FlatString::FlatString(uint32_t len) :
    mLen(len) {}

FlatString*
FlatString::New(GCCapability &gc, uint32_t len) {
    void *mem = gc.alloc_bytes(sizeof(FlatString) + len + 1);
    return new(mem) FlatString(len);
}

void
FlatString::print() const {
    printf("%s", mBuffer);
}

uint32_t
FlatString::length() const {
    return mLen;
}

uint32_t
FlatString::storageSize() const {
    return sizeof(FlatString) + mLen + 1;
}

const char *
FlatString::c_str() const {
    return reinterpret_cast<const char *>(mBuffer);
}

/*
 * ConstString
 **************/

ConstString::ConstString(const char * str) :
    mStr(str) { }

void
ConstString::print() const {
    printf("%s", mStr);
}

uint32_t
ConstString::length() const {
    return strlen(mStr);
}

uint32_t
ConstString::storageSize() const {
    return sizeof(ConstString) + strlen(mStr) + 1;
}

const char *
ConstString::c_str() const {
    return mStr;
}

}  // namespace pz
