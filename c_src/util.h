// This file is part of euv released under the MIT license.
// See the LICENSE file for more information.

#ifndef EUV_UTIL_H
#define EUV_UTIL_H

#include "erl_nif.h"

ERL_NIF_TERM euv_make_atom(ErlNifEnv* env, const char* name);
ERL_NIF_TERM euv_make_ok(ErlNifEnv* env, ERL_NIF_TERM value);
ERL_NIF_TERM euv_make_error(ErlNifEnv* env, ERL_NIF_TERM value);
ERL_NIF_TERM euv_make_error_str(ErlNifEnv* env, const char* reason);

#endif // Included euv.h
