/*
 * Radix tree data structure for symbol lookup
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2016, 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_RADIX_TREE_H
#define PZ_RADIX_TREE_H

#ifdef __cplusplus

#include <vector>

#include "pz_util.h"

namespace pz {

class RadixTreeHelpers
{
  protected:
    static bool
    streq(const char *s1, const char *s2, unsigned len, unsigned *pos);
};

// Forward declare to avoid C++'s problems with cyclic references.
template<typename T>
class RadixTreeNode;
template<typename T>
class RadixTree;

template<typename T>
class RadixTreeEdge {
  private:
    // OPT: Prefixes could share storage, but we either need to determine
    // how to free them or GC must support interior pointers.
    char                   *prefix;
    class RadixTreeNode<T> *node;

    RadixTreeEdge() : prefix(nullptr), node(nullptr) {}
    RadixTreeEdge(char *prefix, class RadixTreeNode<T> *node) :
        prefix(prefix),
        node(node) {}

    ~RadixTreeEdge() {
        if (prefix) {
            free(prefix);
        }
        if (node) {
            delete node;
        }
    }

    friend class RadixTree<T>;
    friend class RadixTreeNode<T>;
};

template<typename T>
class RadixTreeNode : private RadixTreeHelpers {
  private:
    std::vector<RadixTreeEdge<T>*> edges;
    Optional<T>                    data;

    unsigned char                  first_char;

    RadixTreeNode() :
        first_char(0) {}
    RadixTreeNode(RadixTreeEdge<T>* edge, unsigned char char_) :
        edges(1, edge), first_char(char_) {}

    ~RadixTreeNode()
    {
        if (data.hasValue()) {
            Deleter<T>::delete_if_nonnull(data.value());
        }

        for (auto edge : edges) {
            if (edge) {
                delete edge;
            }
        }
    }

    Optional<T>
    lookup(const char *key, unsigned pos) const;

    void
    insert(const char *key, T value, unsigned pos);

    unsigned char
    lastPlus1Char() const {
        return first_char + edges.size();
    }

    void fix_range(unsigned char char_);

    friend class RadixTree<T>;
    friend class RadixTreeEdge<T>;
};

template<typename T>
class RadixTree {
  private:
    RadixTreeNode<T> root;

  public:
    Optional<T> lookup(const char *key) const
    {
        return root.lookup(key, 0);
    }

    void insert(const char *key, T value)
    {
        root.insert(key, value, 0);
    }
};

} // namespace pz

#endif

#endif /* ! PZ_RADIX_TREE_H */
