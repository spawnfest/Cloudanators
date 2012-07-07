// This file is part of euv released under the MIT license.
// See the LICENSE file for more information.

#include <stdio.h>
#include <string.h>

#include "erl_nif.h"

#include "loop.h"
#include "util.h"


#define ATOM(name, value)                       \
do {                                            \
    ATOM_##name = euv_make_atom(env, value);    \
} while(0)


typedef struct {
    euv_loop_t**        loops;
    int                 nloops;
} euv_st;


ERL_NIF_TERM ATOM_OK;
ERL_NIF_TERM ATOM_ERROR;
ERL_NIF_TERM ATOM_LOOPS;
ERL_NIF_TERM ATOM_DEFAULT;


static int
euv_init_loops(ErlNifEnv* env, ERL_NIF_TERM loops, euv_st* st)
{
    ERL_NIF_TERM name;
    unsigned int i;

    if(!enif_is_list(env, loops)) goto error;
    if(!enif_get_list_length(env, loops, &i)) goto error;

    st->nloops = (int) i;
    st->loops = (euv_loop_t**) enif_alloc(st->nloops * sizeof(euv_loop_t*));
    if(st->loops == NULL) goto error;

    memset(st->loops, 0, st->nloops * sizeof(euv_loop_t*));

    i = 0;
    while(enif_get_list_cell(env, loops, &name, &loops)) {
        if(!enif_is_atom(env, name)) goto error;
        st->loops[i++] = euv_loop_init(env, name);
        if(st->loops == NULL) goto error;
    }

    return 1;

error:
    return 0;
}


static void
euv_st_destroy(euv_st* st)
{
    int i;

    if(st == NULL) return;

    if(st->loops != NULL) {
        for(i = 0; i < st->nloops; i++) {
            if(st->loops[i] != NULL) {
                euv_loop_destroy(st->loops[i]);
            }
        }
        enif_free(st->loops);
    }

    if(st != NULL) enif_free(st);
}


static int
euv_load(ErlNifEnv* env, void** priv, ERL_NIF_TERM opts)
{
    ERL_NIF_TERM opt;
    const ERL_NIF_TERM* tuple;
    int arity;

    euv_st* st = (euv_st*) enif_alloc(sizeof(euv_st));
    if(st == NULL) goto error;

    memset(st, 0, sizeof(euv_st));

    ATOM(OK, "ok");
    ATOM(ERROR, "error");
    ATOM(LOOPS, "loops");
    ATOM(DEFAULT, "default");

    if(!enif_is_list(env, opts)) {
        return 1;
    }

    while(enif_get_list_cell(env, opts, &opt, &opts)) {
        if(!enif_get_tuple(env, opt, &arity, &tuple)) goto error;
        if(arity != 2) goto error;

        if(enif_compare(ATOM_LOOPS, tuple[0]) == 0) {
            if(!euv_init_loops(env, tuple[1], st)) goto error;
        }
        // Ignore unknown options
    }

    *priv = (void*) st;

    return 0;

error:
    euv_st_destroy(st);
    return 1;
}


static int
euv_reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}


static int
euv_upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    return euv_load(env, priv, info);
}


static void
euv_unload(ErlNifEnv* env, void* priv)
{
    euv_st* st = (euv_st*) priv;
    euv_st_destroy(st);
    return;
}

static ERL_NIF_TERM
foo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_badarg(env);
}


static ErlNifFunc euv_funcs[] =
{
    {"foo", 0, foo}
};


ERL_NIF_INIT(euv, euv_funcs, &euv_load, &euv_reload, &euv_upgrade, &euv_unload);
