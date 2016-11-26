/*
 * Radix tree data structure for symbol lookup
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2016 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_RADIX_TREE_H
#define PZ_RADIX_TREE_H

typedef struct PZ_RadixTree_Node_Struct PZ_RadixTree;

PZ_RadixTree*
pz_radix_init(void);

void
pz_radix_free(PZ_RadixTree *tree, void(*free_item)(void*));

void *
pz_radix_lookup(PZ_RadixTree *tree, const char * key);

void
pz_radix_insert(PZ_RadixTree *tree, const char * key, void *value);

#endif /* ! PZ_RADIX_TREE_H */
