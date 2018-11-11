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
RadixTreeNode<T>::lookup(const std::string &key, unsigned pos) const
{
    if (pos >= key.size()) {
        return data;
    }

    unsigned char this_char = (unsigned char)key[pos];

    unsigned char index = this_char - first_char;
    if (this_char < lastPlus1Char()) {
        pos++;
        RadixTreeEdge<T> *edge = edges[index];
        if (edge) {
            unsigned prefix_len = edge->prefix.size();
            if (0 == key.compare(pos, prefix_len, edge->prefix)) {
                pos += prefix_len;
                return edge->node.lookup(key, pos);
            }
        }
    }
    return Optional<T>::Nothing();
}

template<typename T>
void
RadixTreeNode<T>::insert(const std::string &key, T value, unsigned pos)
{
    if (pos >= key.size()) {
        assert(!data.hasValue());
        data.set(value);
        return;
    }
    unsigned char char_ = (unsigned char)key[pos];

    if ((char_ < first_char) || (char_ >= lastPlus1Char())) {
        fix_range(char_);
    }

    unsigned char     index = char_ - first_char;
    RadixTreeEdge<T> *edge = edges[index];
    pos++;
    if (edge) {
        unsigned prefix_len = edge->prefix.size();
        unsigned prefix_pos = 0;

        bool match = streq(edge->prefix, std::string(key, pos), &prefix_pos);
        if (match) {
            pos += prefix_len;
        } else {
            char next_char = edge->prefix.at(prefix_pos);
            /*
             * Break the tree and setup a node where the two
             * branches will be created.
             */
            std::string non_matched_part =
                std::string(edge->prefix, prefix_pos + 1);
            std::string matched_part = std::move(edge->prefix);
            matched_part.resize(prefix_pos);
            edge->prefix = std::move(non_matched_part);
            edge = new RadixTreeEdge<T>(std::move(matched_part), next_char,
                    edge);
            edges[index] = edge;
            pos += prefix_pos;
        }
    } else {
        edge = new RadixTreeEdge<T>(std::string(key, pos));
        edges[index] = edge;
        pos += edge->prefix.size();
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

