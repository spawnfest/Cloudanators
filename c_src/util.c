// This file is part of euv released under the MIT license.
// See the LICENSE file for more information.

#include "euv.h"
#include "util.h"


ERL_NIF_TERM
euv_make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;

    if(enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1))
        return ret;

    return enif_make_atom(env, name);
}

ERL_NIF_TERM
euv_make_ok(ErlNifEnv* env, ERL_NIF_TERM value)
{
    return enif_make_tuple2(env, ATOM_OK, value);
}

ERL_NIF_TERM
euv_make_error(ErlNifEnv* env, ERL_NIF_TERM value)
{
    return enif_make_tuple2(env, ATOM_ERROR, value);
}

ERL_NIF_TERM
euv_make_error_str(ErlNifEnv* env, const char* reason)
{
    return euv_make_error(env, euv_make_atom(env, reason));
}
