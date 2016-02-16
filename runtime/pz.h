/*
 * Plasma bytecode in-memory representation
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_H
#define PZ_H

typedef struct PZ_Struct {
    struct PZ_Structs_Struct    *structs;
    struct PZ_Data_Struct       *data;
    struct PZ_Code_Struct       *code;
    /*
     * This field is signed so that it can represent -1 indicating that
     * there is no entry procedure for this module.
     */
    int32_t                     entry_proc;
} PZ;

void pz_free(PZ *pz);

#endif /* ! PZ_H */
