// This file is part of euv released under the MIT license.
// See the LICENSE file for more information.

#ifndef EUV_UTIL_H
#define EUV_UTIL_H

#include "erl_nif.h"

ERL_NIF_TERM euv_make_atom(ErlNifEnv* env, const char* name);
ERL_NIF_TERM euv_make_ok(ErlNifEnv* env, ERL_NIF_TERM value);
ERL_NIF_TERM euv_make_error(ErlNifEnv* env, ERL_NIF_TERM value);
ERL_NIF_TERM euv_make_error_str(ErlNifEnv* env, const char* reason);
ERL_NIF_TERM euv_list_append(ErlNifEnv* env, ERL_NIF_TERM v, ERL_NIF_TERM l);

int euv_pl_lookup(
        ErlNifEnv* env, ERL_NIF_TERM p, ERL_NIF_TERM k, ERL_NIF_TERM* r);

#endif // Included euv.h
