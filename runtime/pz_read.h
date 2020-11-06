/*
 * Plasma bytecode reader
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2916, 2020 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_READ_H
#define PZ_READ_H

namespace pz {

Library *
read(PZ &pz, const std::string &filename);

} // namespace pz

#endif /* ! PZ_READ_H */
