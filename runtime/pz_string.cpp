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

String::String(const BaseString * s) : mStr(s) {}

// XXX LEAK!!
String::String(const char *c_str) : mStr(new ConstString(c_str)) {}

void *
String::ptr() const {
    /*
     * C++ should allow implicit const-cast when the implicit cast is to
     * void*.  You can't write-through a void* anyway, it's the next cast,
     * *FROM* void* that's a problem.
     */
    return const_cast<BaseString *>(mStr);
}

String
String::from_ptr(void *ptr) {
    return String(reinterpret_cast<BaseString*>(ptr));
}

void
String::print() const {
    mStr->print();
}

uint32_t
String::length() const {
    return mStr->length();
}

bool
String::equals(const String &other) const {
    return 0 == strcmp(c_str(), other.c_str());
}

const char *
String::c_str() const {
    return mStr->c_str();
}

String
String::append(GCCapability &gc, const String s1, const String s2) {
    uint32_t len = s1.length() + s2.length() + 1;
    FlatString *s = FlatString::New(gc, len);
    strcpy(reinterpret_cast<char*>(s->buffer()), s1.c_str());
    strcat(reinterpret_cast<char*>(s->buffer()), s2.c_str());
    return String(s);
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
