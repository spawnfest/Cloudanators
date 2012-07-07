// This file is part of euv released under the MIT license.
// See the LICENSE file for more information.

#include "erl_nif.h"

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    return load(env, priv, info);
}

static void
unload(ErlNifEnv* env, void* priv)
{
    enif_free(priv);
    return;
}

static ERL_NIF_TERM
foo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_badarg(env);
}

static ErlNifFunc funcs[] =
{
    {"foo", 0, foo}
};

ERL_NIF_INIT(euv, funcs, &load, &reload, &upgrade, &unload);
