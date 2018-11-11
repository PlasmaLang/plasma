/*
 * Radix tree data structure for symbol lookup
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2016, 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_RADIX_TREE_H
#define PZ_RADIX_TREE_H

#include <vector>

#include "pz_cxx_future.h"

namespace pz {

class RadixTreeHelpers
{
  protected:
    static bool
    streq(const std::string &prefix, const std::string &key, unsigned *pos);
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
    std::string             prefix;
    class RadixTreeNode<T>  node;

    RadixTreeEdge() : prefix(nullptr) {}
    RadixTreeEdge(std::string &&prefix) : prefix(prefix) {}
    RadixTreeEdge(std::string &&prefix,
                  char next_char,
                  RadixTreeEdge<T> *edge) :
        prefix(prefix), node(edge, next_char) {}

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
        for (auto edge : edges) {
            if (edge) {
                delete edge;
            }
        }
    }

    Optional<T>
    lookup(const std::string &key, unsigned pos) const;

    void
    insert(const std::string &key, T value, unsigned pos);

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
    Optional<T> lookup(const std::string &key) const
    {
        return root.lookup(key, 0);
    }

    void insert(const std::string &key, T value)
    {
        root.insert(key, value, 0);
    }
};

} // namespace pz

#endif /* ! PZ_RADIX_TREE_H */
