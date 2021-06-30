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

namespace pz {

// There are no destructors because these are either stack or GC allocated.

class BaseString;

/*
 * Ths string class wraps a reference to the real string.  You should pass it
 * by value rather than pointer or reference.
 */
class String {
  private:
    const BaseString *mStr;

  public:
    explicit String(const BaseString *);
    explicit String(const char *);

    // Get a raw pointer (for the bytecode interpreter).
    void* ptr() const;
    static
    String from_ptr(void*);

    void print() const;
    bool equals(const String &) const;

    // Length in code points
    uint32_t length() const;

    // Length in bytes in RAM, including bookkeeping.
    uint32_t storageSize() const;

    const char * c_str() const;

    static
    String append(GCCapability &gc, const String, const String);
};

class BaseString {
  public:
    virtual void print() const = 0;

    // Length in code points
    virtual uint32_t length() const = 0;

    // Length in bytes in RAM, including bookkeeping.
    virtual uint32_t storageSize() const = 0;

    virtual const char * c_str() const = 0;
};

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

    virtual void print() const;
    virtual uint32_t length() const;
    virtual uint32_t storageSize() const;

    virtual const char * c_str() const;

    uint8_t * buffer() {
        return mBuffer;
    }
    const uint8_t * buffer() const {
        return mBuffer;
    }

    void fixSize(uint32_t len) {
        assert(len <= mLen);
        mLen = len;
    }
};

class ConstString : public BaseString {
  private:
    const char * mStr;

  public:
    // Assumes that the raw data will live longer than this object.
    // Possibly because the
    ConstString(const char *);
    
    virtual void print() const;
    virtual uint32_t length() const;
    virtual uint32_t storageSize() const;

    virtual const char * c_str() const;
};

}  // namespace pz

#endif // ! PZ_String_H
