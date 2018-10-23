/*
 * Radix tree data structure for symbol lookup
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2016, 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_RADIX_TREE_IMPL_H
#define PZ_RADIX_TREE_IMPL_H

#include <stdio.h>
#include <string.h>

#include "pz_common.h"

namespace pz {

template<typename T>
Optional<T>
RadixTreeNode<T>::lookup(const char *key, unsigned pos) const
{
    if (0 == key[pos]) {
        return data;
    }

    unsigned char index = ((unsigned char)key[pos]) - first_char;
    if ((unsigned char)key[pos] < lastPlus1Char()) {
        pos++;
        RadixTreeEdge<T> *edge = edges[index];
        if (edge) {
            assert(edge->prefix);
            unsigned prefix_len = strlen(edge->prefix);
            if (0 == strncmp(edge->prefix, &key[pos], prefix_len)) {
                pos += prefix_len;
                return edge->node.lookup(key, pos);
            }
        }
    }
    return Optional<T>::Nothing();
}

template<typename T>
void
RadixTreeNode<T>::insert(const char *key, T value, unsigned pos)
{
    unsigned char char_ = (unsigned char)key[pos];
    if (0 == char_) {
        assert(!data.hasValue());
        data.set(value);
        return;
    }

    if ((char_ < first_char) || (char_ >= lastPlus1Char())) {
        fix_range(char_);
    }

    unsigned char     index = char_ - first_char;
    RadixTreeEdge<T> *edge = edges[index];
    pos++;
    if (edge) {
        unsigned prefix_len = strlen(edge->prefix);
        unsigned prefix_pos = 0;

        bool match = streq(edge->prefix, &key[pos], prefix_len, &prefix_pos);
        char next_char = edge->prefix[prefix_pos];
        if (match) {
            pos += prefix_len;
        } else {
            /*
             * Break the tree and setup a node where the two
             * branches will be created.
             */
            char *matched_part = edge->prefix;
            matched_part[prefix_pos] = 0;
            char *non_matched_part =
                strdup(&(edge->prefix[prefix_pos + 1]));
            edge->prefix = non_matched_part;
            edge = new RadixTreeEdge<T>(matched_part, next_char, edge);
            edges[index] = edge;
            pos += prefix_pos;
        }
    } else {
        edge = new RadixTreeEdge<T>(strdup(&key[pos]));
        edges[index] = edge;
        pos += strlen(edge->prefix);
    }
    edge->node.insert(key, value, pos);
}

template<typename T>
void
RadixTreeNode<T>::fix_range(unsigned char char_)
{
    if (edges.empty()) {
        edges.resize(1);
        first_char = char_;
    } else if (char_ < first_char) {
        unsigned char last_plus_1 = lastPlus1Char();
        edges.resize(last_plus_1 - char_);

        unsigned char shift = first_char - char_;
        for (int i = last_plus_1 - first_char - 1; i >= 0; i--) {
            // Should move?
            edges.at(i + shift) = edges.at(i);
        }
        for (unsigned char i = 0; i < shift; i++) {
            edges.at(i) = nullptr;
        }
        first_char = char_;
    } else if (char_ >= lastPlus1Char()) {
        // Add 1 since the end bound of our array is exclusive.
        unsigned char_plus_1 = char_ + 1;
        edges.resize(char_plus_1 - first_char);
    } else {
        fprintf(stderr, "Tree doesn't need widening");
        abort();
    }
}

} // namespace pz

#endif // ! PZ_RADIX_TREE_IMPL_H

