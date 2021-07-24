/*
 * Plasma strings
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2021 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include <string.h>

#include "pz_common.h"
#include "pz_gc.h"

#include "pz_string.h"

namespace pz {

static void
AssertAligned(const void *p) {
    assert((reinterpret_cast<uintptr_t>(p) & HIGH_TAG_MASK) == 0);
}

String::String() :
    mType(ST_EMPTY)
{
    s.cStr = nullptr;
}

String::String(const BaseString * base_str) :
    mType(ST_FLAT)
{
    // Pointers must be aligned
    AssertAligned(base_str);
    s.baseStr = base_str;
}

String::String(const char *c_str) :
    mType(ST_CONST)
{
    // Pointers must be aligned
    AssertAligned(c_str);
    s.cStr = c_str;
}

void *
String::ptr() const {
    if (mType == ST_EMPTY) {
        return nullptr;
    } else {
        return reinterpret_cast<void*>(
                reinterpret_cast<uintptr_t>(s.cStr) | 
                (static_cast<uintptr_t>(mType) << HIGH_TAG_SHIFT));
    }
}

String
String::from_ptr(void *ptr) {
    if (ptr == nullptr) {
        return String();
    } else {
        StringType tag = static_cast<StringType>(
            (reinterpret_cast<uintptr_t>(ptr) & HIGH_TAG_MASK) >> HIGH_TAG_SHIFT);
        uintptr_t pointer_no_tag =
            reinterpret_cast<uintptr_t>(ptr) & ~HIGH_TAG_MASK;

        switch (tag) {
            case ST_FLAT:
                return String(reinterpret_cast<BaseString*>(pointer_no_tag));
            case ST_CONST:
                return String(reinterpret_cast<const char *>(pointer_no_tag));
            default:
                abort();
        }
    }
}

void
String::print() const {
    switch (mType) {
        case ST_CONST:
            printf("%s", s.cStr);
            break;
        case ST_FLAT:
            s.baseStr->print();
            break;
        case ST_EMPTY:
            break;
    }
}

uint32_t
String::length() const {
    switch (mType) {
        case ST_CONST:
            return strlen(s.cStr);
        case ST_FLAT:
            return s.baseStr->length();
        case ST_EMPTY:
            return 0;
        default:
            abort();
    }
}

bool
String::equals(const String &other) const {
    return equals_pointer(other) || (0 == strcmp(c_str(), other.c_str()));
}

bool
String::equals_pointer(const String &other) const {
    return this->s.cStr == other.s.cStr;
}

const char *
String::c_str() const {
    switch (mType) {
        case ST_CONST:
            return s.cStr;
        case ST_FLAT:
            return s.baseStr->c_str();
        case ST_EMPTY:
            return "";
        default:
            abort();
    }
}

uint32_t
String::char_at(unsigned i) const {
    assert(i < length());
    // XXX make better.
    return c_str()[i];
}

size_t
String::hash() const {
    const char *c = c_str();
    size_t hash = 0;

    for (unsigned i = 0; i < length(); i++) {
        hash = (hash >> (sizeof(size_t)*8-1) | hash << 1) ^
            std::hash<char>{}(c[i]);
    }

    return hash;
}

String
String::append(GCCapability &gc, const String s1, const String s2) {
    uint32_t len = s1.length() + s2.length();
    if (len == 0) {
        return String();
    } else {
        FlatString *s = FlatString::New(gc, len);
        strcpy(s->buffer(), s1.c_str());
        strcat(s->buffer(), s2.c_str());
        return String(s);
    }
}
    
String
String::substring(GCCapability &gc, const StringPos * pos1,
        const StringPos * pos2)
{
    assert(pos1->mPos <= pos1->mStr.length());
    assert(pos2->mPos <= pos2->mStr.length());

    if (!pos1->mStr.equals_pointer(pos2->mStr)) {
        fprintf(stderr, "Substring for two different strings\n");
        exit(1);
    }

    int len = pos2->mPos - pos1->mPos;
    if (len <= 0) {
        return String();
    }

    FlatString *s = FlatString::New(gc, len);
    strncpy(s->buffer(), &pos1->mStr.c_str()[pos1->mPos], len);
    s->fixSize(len);
    return String(s);
}

String
String::dup(GCCapability &gc, const std::string & str)
{
    uint32_t len = str.length();
    if (len == 0) {
        return String();
    } else {
        FlatString *s = FlatString::New(gc, len);
        strcpy(s->buffer(), str.c_str());
        return String(s);
    }
}

bool
String::operator==(const String other) const
{
    return equals(other);
}

StringPos*
String::begin(GCCapability &gc) const {
    return new(gc) StringPos(*this, 0);
}

StringPos*
String::end(GCCapability &gc) const {
    // XXX won't work with other encodings.
    return new(gc) StringPos(*this, length());
}

/*
 * FlatString
 *************/

FlatString::FlatString(uint32_t len) :
    mLen(len)
{
    assert(len > 0);
    // It starts as an empty, null terminated string.
    mBuffer[0] = 0;
}

FlatString*
FlatString::New(GCCapability &gc, uint32_t len) {
    void *mem = gc.alloc_bytes(sizeof(FlatString) + len + 1);
    return new(mem) FlatString(len);
}

StringType
FlatString::type() const{
    return ST_FLAT;
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
 * FlatString
 *************/

bool
StringPos::at_beginning() const {
    return mPos == 0;
}

bool
StringPos::at_end() const {
    return mPos == mStr.length();
}

StringPos *
StringPos::forward(GCCapability &gc) const {
    if (at_end()) {
        fprintf(stderr, "StringPos already at end\n");
        exit(1);
    }
    return new(gc) StringPos(mStr, mPos+1);
}

StringPos *
StringPos::backward(GCCapability &gc) const {
    if (at_beginning()) {
        fprintf(stderr, "StringPos already at beginning");
        exit(1);
    }
    return new(gc) StringPos(mStr, mPos-1);
}

uint32_t
StringPos::next_char() const {
    if (mPos == mStr.length()) {
        fprintf(stderr, "Access next character at end of string\n");
        exit(1);
    }
    return mStr.char_at(mPos);
}

uint32_t
StringPos::prev_char() const {
    if (mPos == 0) {
        fprintf(stderr, "Access previous character at beginning of string\n");
        exit(1);
    }
    return mStr.char_at(mPos - 1);
}

}  // namespace pz

namespace std
{
    size_t
    hash<pz::String>::operator()(pz::String const& s) const noexcept
    {
        return s.hash();
    }
}

