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
    strneq(const char *s1, const char *s2, unsigned len, unsigned *pos);
};

template<typename T>
class RadixTree : private RadixTreeHelpers {
  private:
    class Node;

    class Edge {
        // OPT: Prefixes could share storage, but we either need to determine
        // how to free them or GC must support interior pointers.
        char       *prefix;
        class Node *node;

        Edge() : prefix(nullptr), node(nullptr) {}
        Edge(char *prefix, class Node *node) :
            prefix(prefix),
            node(node) {}

        ~Edge() {
            if (prefix) {
                free(prefix);
            }
            if (node) {
                delete node;
            }
        }

        friend class RadixTree;
    };

    class Node {
      private:
        // OPT: make edges part of this structure to decrease pointer following,
        std::vector<Edge*> edges;
        T                  data;

        unsigned char      first_char;

        Node() :
            data(0),
            first_char(0) {}

        ~Node()
        {
            Deleter<T>::delete_if_nonnull(data);

            for (auto edge : edges) {
                if (edge) {
                    delete edge;
                }
            }
        }

        unsigned char
        lastPlus1Char() const {
            return first_char + edges.size();
        }

        void fix_range(unsigned char char_);

        friend class RadixTree;
    };

    Node root;

  public:
    RadixTree() : root(Node()) {}

    T lookup(const char *key);

    void insert(const char *key, T value);
};

} // namespace pz

#endif

#endif /* ! PZ_RADIX_TREE_H */
