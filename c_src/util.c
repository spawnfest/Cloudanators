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
    return enif_make_tuple2(env, EUV_ATOM_OK, value);
}


ERL_NIF_TERM
euv_make_error(ErlNifEnv* env, ERL_NIF_TERM value)
{
    return enif_make_tuple2(env, EUV_ATOM_ERROR, value);
}


ERL_NIF_TERM
euv_make_error_str(ErlNifEnv* env, const char* reason)
{
    return euv_make_error(env, euv_make_atom(env, reason));
}


ERL_NIF_TERM
euv_list_append(ErlNifEnv* env, ERL_NIF_TERM i, ERL_NIF_TERM l)
{
    return enif_make_list_cell(env, i, l);
}


int
euv_pl_lookup(ErlNifEnv* env, ERL_NIF_TERM p, ERL_NIF_TERM k, ERL_NIF_TERM* r)
{
    ERL_NIF_TERM pair;
    const ERL_NIF_TERM* tuple;
    int arity;

    while(enif_get_list_cell(env, p, &pair, &p)) {
        if(!enif_get_tuple(env, pair, &arity, &tuple))
            continue;
        if(arity != 2)
            continue;
        if(enif_compare(k, tuple[0]) == 0) {
            *r = tuple[1];
            return 1;
        }
    }
    return 0;
}
