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
RadixTree<T>::lookup_helper(RadixTree *tree, const char *key)
{
    unsigned pos = 0;

    while (0 != key[pos]) {
        unsigned char  index;
        Edge          *edge;

        index = ((unsigned char)key[pos]) - tree->first_char;
        if ((unsigned char)key[pos] < tree->last_plus_1_char) {
            pos++;
            edge = &(tree->edges[index]);
            if (edge->prefix) {
                unsigned prefix_len = strlen(edge->prefix);
                if (0 == strncmp(edge->prefix, &key[pos], prefix_len)) {
                    pos += prefix_len;
                    tree = edge->node;
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

    return tree->data;
}

template<typename T>
void
RadixTree<T>::insert(const char *key, T value)
{
    insert_helper(this, key, value);
}

template<typename T>
void
RadixTree<T>::insert_helper(RadixTree *tree, const char *key, T value)
{
    unsigned    pos = 0;
    const char *orig_key = key;

    while (0 != key[pos]) {
        unsigned char  index;
        unsigned char  char_;
        Edge          *edge;

        char_ = (unsigned char)key[pos];
        if ((char_ >= tree->first_char) &&
              (char_ < tree->last_plus_1_char))
        {
            index = char_ - tree->first_char;
            pos++;
            edge = &(tree->edges[index]);
            if (edge->prefix) {
                unsigned prefix_len = strlen(edge->prefix);
                unsigned prefix_pos = 0;

                if (strneq(edge->prefix, &key[pos], prefix_len,
                        &prefix_pos))
                {
                    pos += prefix_len;
                    tree = edge->node;
                } else {
                    RadixTree    *old_node;
                    char         *non_matched_part;
                    char          next_char;

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
                        edge->node = new RadixTree();
                        /*
                         * Add the second part of the broken edge, and the
                         * removed node back into the tree.
                         */
                        edge->node->first_char = next_char;
                        edge->node->last_plus_1_char = next_char + 1;
                        edge->node->edges = malloc(sizeof(Edge));
                        edge->node->edges[0].prefix = non_matched_part;
                        edge->node->edges[0].node = old_node;
                        tree = edge->node;
                        edge = &(edge->node->edges[0]);
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
                edge->prefix = strdup(&key[pos]);
                edge->node = new RadixTree();
                tree = edge->node;
                break; // GO straight to updating the node.
            }
        } else {
            /*
             * Fix the range and let the while loop attempt to insert the
             * item again.
             */
            fix_range(tree, char_);
        }
    }

    if (tree->data == NULL) {
        tree->data = value;
    } else {
        fprintf(stderr, "Collision for %s pz_radix_insert", orig_key);
        abort();
    }
}

template<typename T>
void
RadixTree<T>::fix_range(RadixTree<T> *tree, unsigned char char_)
{
    Edge *new_edges;

    if (NULL == tree->edges) {
        tree->edges = malloc(sizeof(Edge));
        memset(tree->edges, 0, sizeof(Edge));
        tree->first_char = char_;
        tree->last_plus_1_char = char_ + 1;
    } else if (char_ < tree->first_char) {
        new_edges = malloc(sizeof(Edge) *
                           (tree->last_plus_1_char - char_));

        memset(new_edges, 0, sizeof(Edge) *
                               (tree->first_char - char_));
        memcpy(&new_edges[tree->first_char - char_], tree->edges,
               sizeof(Edge) *
                 (tree->last_plus_1_char - tree->first_char));

        free(tree->edges);
        tree->edges = new_edges;
        tree->first_char = char_;
    } else if (char_ >= tree->last_plus_1_char) {
        // Add 1 since the end bound of our array is exclusive.
        unsigned char_plus_1 = char_ + 1;

        tree->edges =
          realloc(tree->edges, sizeof(Edge) *
                                 (char_plus_1 - tree->first_char));
        memset(&(tree->edges[tree->last_plus_1_char - tree->first_char]), 0,
               sizeof(Edge) *
                 (char_plus_1 - tree->last_plus_1_char));
        tree->last_plus_1_char = char_plus_1;
    } else {
        fprintf(stderr, "Tree doesn't need widening");
        abort();
    }
}

} // namespace pz

#endif // ! PZ_RADIX_TREE_IMPL_H

