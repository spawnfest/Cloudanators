// This file is part of euv released under the MIT license.
// See the LICENSE file for more information.

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "erl_nif.h"

#include "euv.h"
#include "loop.h"
#include "util.h"


typedef struct {
    euv_loop_t**        loops;
    int                 nloops;
} euv_st;


ErlNifResourceType* HANDLE_RES;


#define EUV_ATOM_DECL(name, val) ERL_NIF_TERM EUV_ATOM_##name;
EUV_ATOM_TABLE_MAP(EUV_ATOM_DECL)
#undef EUV_ATOM_DECL


#define EUV_ATOM_INST(name, val) EUV_ATOM_##name = euv_make_atom(env, val);
static void
euv_init_atoms(ErlNifEnv* env)
{
    EUV_ATOM_TABLE_MAP(EUV_ATOM_INST);
}
#undef EUV_ATOM_INST


static int
euv_init_resources(ErlNifEnv* env)
{
    int flags = ERL_NIF_RT_CREATE; // Lets see what no takeover does
    HANDLE_RES = enif_open_resource_type(
            env, NULL, "euv_handle_res", euv_handle_destroy, flags, NULL);
    if(HANDLE_RES == NULL)
        return 0;
    return 1;
}


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


static euv_loop_t*
euv_find_loop(ErlNifEnv* env, ERL_NIF_TERM name)
{
    euv_st* st = (euv_st*) enif_priv_data(env);
    int i;

    assert(st->loops != NULL && "invalid loops list");
    assert(st->nloops > 0 && "invalid loop count");

    for(i = 0; i < st->nloops; i++) {
        if(euv_loop_is(st->loops[i], env, name))
            return st->loops[i];
    }

    return st->loops[0];
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

    euv_st* st = (euv_st*) enif_alloc(sizeof(euv_st));
    if(st == NULL) goto error;

    memset(st, 0, sizeof(euv_st));

    euv_init_atoms(env);

    if(!euv_init_resources(env))
        return 1;

    if(!enif_is_list(env, opts))
        return 1;

    if(euv_pl_lookup(env, opts, EUV_ATOM_LOOPS, &opt)) {
        if(!euv_init_loops(env, opt, st))
            goto error;
    } else {
        goto error;
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
euv_submit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    euv_handle_t* res;
    euv_loop_t* loop = NULL;
    ERL_NIF_TERM opt;
    ERL_NIF_TERM opts;
    ERL_NIF_TERM p1;
    ERL_NIF_TERM p2;
    euv_req_t* req = euv_req_init();

    enif_self(env, &req->pid);

    if(argc == 3) {
        if(!enif_is_ref(env, argv[0]))
            goto error;
        if(!enif_is_list(env, argv[1]))
            goto error;
        if(!enif_is_list(env, argv[2]))
            goto error;
        req->ref = enif_make_copy(req->env, argv[0]);
        req->args = enif_make_copy(req->env, argv[1]);
        opts = argv[2];
    } else if(argc == 4) {
        if(!enif_is_ref(env, argv[0]))
            goto error;
        if(!enif_get_resource(env, argv[1], HANDLE_RES, (void**) &res))
            goto error;
        if(!enif_is_list(env, argv[2]))
            goto error;
        if(!enif_is_list(env, argv[3]))
            goto error;
        req->ref = enif_make_copy(req->env, argv[0]);
        req->handle = res;
        req->args = enif_make_copy(req->env, argv[2]);
        opts = argv[3];
    } else {
        goto error;
    }

    // Find the loop to use when we don't have
    // a handle.
    if(req->handle == NULL) {
        if(euv_pl_lookup(env, opts, EUV_ATOM_LOOP, &opt)) {
            loop = euv_find_loop(env, opt);
        } else {
            // No loop specified, choose the default
            loop = euv_find_loop(env, EUV_ATOM_DEFAULT);
        }
    } else {
        loop = req->handle->loop;
        p1 = enif_make_pid(env, &(req->pid));
        p2 = enif_make_pid(env, &(req->handle->pid));
        if(enif_compare(p1, p2) != 0)
            goto error;
    }

    req->type = euv_req_get_type(env, opts);
    if(req->type == EUV_REQ_UNKNOWN)
        goto error;

    if(!euv_loop_queue(loop, req))
        goto error;

    return EUV_ATOM_OK;

error:
    euv_req_destroy(req);
    return enif_make_badarg(env);
}


static ErlNifFunc euv_funcs[] =
{
    {"submit", 3, euv_submit},
    {"submit", 4, euv_submit}
};


ERL_NIF_INIT(euv, euv_funcs, &euv_load, &euv_reload, &euv_upgrade, &euv_unload);
