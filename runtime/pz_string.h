/*
 * Plasma strings
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2021 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_String_H
#define PZ_String_H

#include "pz_gc_util.h"

#include <string>

namespace pz {

class BaseString;
class StringPos;

enum StringType : uint8_t {
    ST_FLAT = 0,
    ST_CONST,
    ST_EMPTY
};

/*
 * Ths string class wraps a reference to the real string.  You should pass it
 * by value rather than pointer or reference.
 */
class String {
  private:
    union {
        const BaseString   *baseStr;
        const char         *cStr;
    } s;
    StringType mType;

  public:
    String();
    explicit String(const BaseString *);
    explicit String(const char *);

    // Get a raw pointer (for the bytecode interpreter).
    void* ptr() const;
    static
    String from_ptr(void*);

  protected:
    // GC ptr-ptr.  Get a pointer to the pointer, for GC rooting.
    const void ** gc_ptr() {
        switch (mType) {
          case ST_EMPTY:
          case ST_CONST:
            return nullptr;
          case ST_FLAT:
            return reinterpret_cast<const void**>(&s.baseStr);
          default:
            fprintf(stderr, "Invalid string type");
            abort();
        }
    };

  public:
    void print() const;
    bool equals(const String &) const;
    bool equals_pointer(const String &) const;
    bool startsWith(const String &, GCCapability &gc) const;

    // Length in code points
    uint32_t length() const;

    // Length in bytes in RAM, including bookkeeping.
    uint32_t storageSize() const;

    const char * c_str() const;

    // Get the character at this raw position.
    uint32_t char_at(unsigned i) const;

    size_t hash() const;

    static
    String append(GCCapability &gc, const String, const String);

    static
    String substring(GCCapability &gc, const StringPos * pos1,
            const StringPos * pos2);

    static
    String dup(GCCapability &gc, const std::string &str);

    bool operator==(const String string) const;

    StringPos* begin(GCCapability &gc) const;
    StringPos* end(GCCapability &gc) const;
};

class RootString : public String {
  private:
    GCTracer & m_tracer;

  public:
    RootString(GCTracer & gc, String && str) : String(str), m_tracer(gc) {
        m_tracer.add_root(gc_ptr());
    }
    ~RootString() {
        m_tracer.remove_root(gc_ptr());
    }
};

class BaseString {
  public:
    virtual StringType type() const = 0;

    virtual void print() const = 0;

    // Length in code points
    virtual uint32_t length() const = 0;

    // Length in bytes in RAM, including bookkeeping.
    virtual uint32_t storageSize() const = 0;

    virtual const char * c_str() const = 0;
};

// A flat string has both a null-terminating byte and a length field.
class FlatString : public BaseString {
  private:
    uint32_t    mLen;
    uint8_t     mBuffer[];

  protected:
    FlatString(uint32_t len);

  public:
    // We don't use the GCNew class' placement new because we need custom
    // lengths.
    static FlatString* New(GCCapability &gc, uint32_t len);

    virtual StringType type() const;

    virtual void print() const;
    virtual uint32_t length() const;
    virtual uint32_t storageSize() const;

    virtual const char * c_str() const;

    char * buffer() {
        return reinterpret_cast<char*>(mBuffer);
    }
    const char * buffer() const {
        return reinterpret_cast<const char*>(mBuffer);
    }

    void fixSize(uint32_t len);
};

class StringPos : public GCNew {
    const String    mStr;
    unsigned        mPos;

  public:
    StringPos(const String &str, unsigned pos) :
        mStr(str), mPos(pos) {}

    bool at_beginning() const;
    bool at_end() const;

    StringPos* forward(GCCapability &gc) const;
    StringPos* backward(GCCapability &gc) const;

    uint32_t next_char() const;
    uint32_t prev_char() const;

    friend class String;
};

}  // namespace pz

namespace std
{
    template<> struct hash<pz::String>
    {
        size_t operator()(pz::String const& s) const noexcept;
    };
}

#endif // ! PZ_String_H
