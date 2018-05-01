/*
 * Radix tree
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include <stdio.h>
#include <string.h>

#include "pz_common.h"

#include "pz_radix_tree.h"

/*
 * This is probably not the most efficient radix tree.  There will be a
 * number of easy to improve things, but before we can do that we must
 * benchmark the tree as part the Plasma runtime and also determine whether
 * a radix tree is indeed the best data structure.
 */

struct PZ_RadixTree_Node_Struct {
    // OPT: Order these after the other fields to avoid padding.
    unsigned char                   first_char;
    unsigned char                   last_plus_1_char;
    // OPT: make edges part of this structure to decrease pointer following,
    struct PZ_RadixTree_Edge_Struct *edges;
    void                            *data;
};

struct PZ_RadixTree_Edge_Struct {
    // OPT: Prefixes could share storage, but we either need to determine
    // how to free them or GC must support interior pointers.
    char                            *prefix;
    struct PZ_RadixTree_Node_Struct *node;
};

PZ_RadixTree *
pz_radix_init(void)
{
    PZ_RadixTree *tree;

    tree = malloc(sizeof(PZ_RadixTree));
    tree->first_char = 0;
    tree->last_plus_1_char = 0;
    tree->edges = NULL;
    tree->data = NULL;

    return tree;
}

void
pz_radix_free(PZ_RadixTree *tree, free_fn free_item)
{
    unsigned char i;

    if ((NULL != free_item) && (NULL != tree->data)) {
        free_item(tree->data);
    }

    if (NULL != tree->edges) {
        for (i = 0; i < (tree->last_plus_1_char - tree->first_char); i++) {
            if (NULL != tree->edges[i].prefix) {
                free(tree->edges[i].prefix);
            }
            if (NULL != tree->edges[i].node) {
                pz_radix_free(tree->edges[i].node, free_item);
            }
        }
        free(tree->edges);
    }

    free(tree);
}

void *
pz_radix_lookup(PZ_RadixTree *tree, const char *key)
{
    unsigned pos = 0;

    while (0 != key[pos]) {
        unsigned char                    index;
        struct PZ_RadixTree_Edge_Struct *edge;

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

static void
fix_range(PZ_RadixTree *tree, unsigned char char_);

static bool
radix_strneq(const char *s1, const char *s2, unsigned len, unsigned *pos);

void
pz_radix_insert(PZ_RadixTree *tree, const char *key, void *value)
{
    unsigned    pos = 0;
    const char *orig_key = key;

    while (0 != key[pos]) {
        unsigned char                    index;
        unsigned char                    char_;
        struct PZ_RadixTree_Edge_Struct *edge;

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

                if (radix_strneq(edge->prefix, &key[pos], prefix_len,
                        &prefix_pos))
                {
                    pos += prefix_len;
                    tree = edge->node;
                } else {
                    PZ_RadixTree *old_node;
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
                        edge->node = pz_radix_init();
                        /*
                         * Add the second part of the broken edge, and the
                         * removed node back into the tree.
                         */
                        edge->node->first_char = next_char;
                        edge->node->last_plus_1_char = next_char + 1;
                        edge->node->edges =
                          malloc(sizeof(struct PZ_RadixTree_Edge_Struct));
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
                edge->node = pz_radix_init();
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

static void
fix_range(PZ_RadixTree *tree, unsigned char char_)
{
    struct PZ_RadixTree_Edge_Struct *new_edges;

    if (NULL == tree->edges) {
        tree->edges = malloc(sizeof(struct PZ_RadixTree_Edge_Struct));
        memset(tree->edges, 0, sizeof(struct PZ_RadixTree_Edge_Struct));
        tree->first_char = char_;
        tree->last_plus_1_char = char_ + 1;
    } else if (char_ < tree->first_char) {
        new_edges = malloc(sizeof(struct PZ_RadixTree_Edge_Struct) *
                           (tree->last_plus_1_char - char_));

        memset(new_edges, 0, sizeof(struct PZ_RadixTree_Edge_Struct) *
                               (tree->first_char - char_));
        memcpy(&new_edges[tree->first_char - char_], tree->edges,
               sizeof(struct PZ_RadixTree_Edge_Struct) *
                 (tree->last_plus_1_char - tree->first_char));

        free(tree->edges);
        tree->edges = new_edges;
        tree->first_char = char_;
    } else if (char_ >= tree->last_plus_1_char) {
        // Add 1 since the end bound of our array is exclusive.
        unsigned char_plus_1 = char_ + 1;

        tree->edges =
          realloc(tree->edges, sizeof(struct PZ_RadixTree_Edge_Struct) *
                                 (char_plus_1 - tree->first_char));
        memset(&(tree->edges[tree->last_plus_1_char - tree->first_char]), 0,
               sizeof(struct PZ_RadixTree_Edge_Struct) *
                 (char_plus_1 - tree->last_plus_1_char));
        tree->last_plus_1_char = char_plus_1;
    } else {
        fprintf(stderr, "Tree doesn't need widening");
        abort();
    }
}

static bool
radix_strneq(const char *s1, const char *s2, unsigned len, unsigned *ret_pos)
{
    unsigned pos;

    for (pos = 0; pos < len; pos++) {
        if (s1[pos] != s2[pos]) {
            *ret_pos = pos;
            return false;
        } else if (s1[pos] == 0) {
            /* Both strings ended together */
            *ret_pos = pos;
            return true;
        }
    }

    /*
     * Stop comparing as this is the end of the prefix in the tree.
     */
    *ret_pos = pos;
    return true;
}
