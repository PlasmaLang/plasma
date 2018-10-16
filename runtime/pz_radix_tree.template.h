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
T
RadixTree<T>::lookup(const char *key)
{
    unsigned pos = 0;
    Node *node = &root;

    while (0 != key[pos]) {
        unsigned char  index;
        Edge          *edge;

        index = ((unsigned char)key[pos]) - node->first_char;
        if ((unsigned char)key[pos] < node->last_plus_1_char) {
            pos++;
            edge = node->edges[index];
            if (edge->prefix) {
                unsigned prefix_len = strlen(edge->prefix);
                if (0 == strncmp(edge->prefix, &key[pos], prefix_len)) {
                    pos += prefix_len;
                    node = edge->node;
                } else {
                    return NULL;
                }
            } else {
                return NULL;
            }
        } else {
            return NULL;
        }
    }

    return node->data;
}

template<typename T>
void
RadixTree<T>::insert(const char *key, T value)
{
    unsigned    pos = 0;
    const char *orig_key = key;
    Node *node = &root;

    while (0 != key[pos]) {
        unsigned char  index;
        unsigned char  char_;
        Edge          *edge;

        char_ = (unsigned char)key[pos];
        if ((char_ >= node->first_char) &&
              (char_ < node->last_plus_1_char))
        {
            index = char_ - node->first_char;
            pos++;
            edge = node->edges[index];
            if (edge) {
                unsigned prefix_len = strlen(edge->prefix);
                unsigned prefix_pos = 0;

                if (strneq(edge->prefix, &key[pos], prefix_len,
                        &prefix_pos))
                {
                    pos += prefix_len;
                    node = edge->node;
                } else {
                    Node  *old_node;
                    char  *non_matched_part;
                    char   next_char;

                    /*
                     * The prefix within the edge only partially matches.
                     * break it into the matching and non-matching parts and
                     * re-add the matching part before adding the new node.
                     *
                     * First save information that we need.
                     */
                    old_node = edge->node;
                    next_char = edge->prefix[prefix_pos];
                    if (next_char == 0) {
                        non_matched_part = NULL;
                    } else {
                        non_matched_part =
                          strdup(&(edge->prefix[prefix_pos + 1]));
                    }
                    /*
                     * If next_char == 0 then we know that the tree's
                     * existing branch is already in the right place and we
                     * don't need to do anything for this step.
                     */
                    if (next_char != 0) {
                        /*
                         * Break the tree and setup a node where the two
                         * branches will be created.
                         */
                        edge->prefix[prefix_pos] = 0;
                        edge->node = new Node();
                        /*
                         * Add the second part of the broken edge, and the
                         * removed node back into the tree.
                         */
                        edge->node->first_char = next_char;
                        edge->node->last_plus_1_char = next_char + 1;
                        edge->node->edges.push_back(
                                    new Edge(non_matched_part, old_node));
                        node = edge->node;
                        edge = edge->node->edges[0];
                    }

                    /*
                     * We've now introduced a new node into the tree, where
                     * we can create a breanch in order to support the new
                     * entry that we're trying to insert.  We can let the
                     * loop continue to make a new attempt to insert the
                     * node.  But first we need to update the pos counter.
                     */
                    pos += prefix_pos;
                }
            } else {
                edge = new Edge(strdup(&key[pos]), new Node());
                node->edges[index] = edge;
                node = edge->node;
                break; // GO straight to updating the node.
            }
        } else {
            /*
             * Fix the range and let the while loop attempt to insert the
             * item again.
             */
            node->fix_range(char_);
        }
    }

    if (node->data == NULL) {
        node->data = value;
    } else {
        fprintf(stderr, "Collision for %s pz_radix_insert", orig_key);
        abort();
    }
}

template<typename T>
void
RadixTree<T>::Node::fix_range(unsigned char char_)
{
    if (edges.empty()) {
        edges.resize(1);
        first_char = char_;
        last_plus_1_char = char_ + 1;
    } else if (char_ < first_char) {
        edges.resize(last_plus_1_char - char_);

        unsigned char shift = first_char - char_;
        for (int i = last_plus_1_char - first_char - 1; i >= 0; i--) {
            // Should move?
            edges.at(i + shift) = edges.at(i);
        }
        for (unsigned char i = 0; i < shift; i++) {
            edges.at(i) = nullptr;
        }
        first_char = char_;
    } else if (char_ >= last_plus_1_char) {
        // Add 1 since the end bound of our array is exclusive.
        unsigned char_plus_1 = char_ + 1;
        edges.resize(char_plus_1 - first_char);
        last_plus_1_char = char_plus_1;
    } else {
        fprintf(stderr, "Tree doesn't need widening");
        abort();
    }
}

} // namespace pz

#endif // ! PZ_RADIX_TREE_IMPL_H

