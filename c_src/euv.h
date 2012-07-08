// This file is part of euv released under the MIT license.
// See the LICENSE file for more information.

#ifndef EUV_H
#define EUV_H

#include "erl_nif.h"

extern ErlNifResourceType* HANDLE_RES;

#define EUV_ATOM_TABLE_MAP(XX)              \
    XX(ATIME,           "atime")            \
    XX(BADARG,          "badarg")           \
    XX(BLOCKS,          "blocks")           \
    XX(BLKSIZE,         "blksize")          \
    XX(CTIME,           "ctime")            \
    XX(DEFAULT,         "default")          \
    XX(DEV,             "dev")              \
    XX(ERROR,           "error")            \
    XX(FLAGS,           "flags")            \
    XX(GEN,             "gen")              \
    XX(GID,             "gid")              \
    XX(INO,             "ino")              \
    XX(INTERNAL_ERROR,  "internal_error")   \
    XX(INVALID_REQ,     "invalid_req")      \
    XX(LOOP,            "loop")             \
    XX(LOOPS,           "loops")            \
    XX(MODE,            "mode")             \
    XX(MTIME,           "mtime")            \
    XX(NLINK,           "nlink")            \
    XX(NOMEM,           "nomem")            \
    XX(OK,              "ok")               \
    XX(PATH,            "path")             \
    XX(RDEV,            "rdev")             \
    XX(REQ,             "req")              \
    XX(SIZE,            "size")             \
    XX(UID,             "uid")

#define EUV_ATOM_EXTERN(name, val) extern ERL_NIF_TERM EUV_ATOM_##name;
EUV_ATOM_TABLE_MAP(EUV_ATOM_EXTERN)
#undef EUV_ATOM_EXTERN

#endif // Included euv.h
