// This file is part of euv released under the MIT license.
// See the LICENSE file for more information.

#include <stdio.h>
#include <string.h>

#include "erl_nif.h"
#include "uv.h"

#include "loop.h"
#include "queue.h"


struct euv_loop_s {
    char                name[256];

    ErlNifTid           tid;
    ErlNifThreadOpts*   opts;

    uv_loop_t*          uvl;
    uv_async_t*         wakeup;
    euv_queue_t*        reqs;

    // Only needed during thread start and stop
    ErlNifMutex*        lock;
    ErlNifCond*         cond;
    int                 ready;
};


void* euv_loop_main(void* arg);


euv_loop_t*
euv_loop_init(ErlNifEnv* env, ERL_NIF_TERM name)
{
    int ready;
    euv_loop_t* loop = enif_alloc(sizeof(struct euv_loop_s));
    if(loop == NULL) goto error;

    memset(loop, 0, sizeof(struct euv_loop_s));

    // Initialize our loop structure

    if(!enif_get_atom(env, name, loop->name, 256, ERL_NIF_LATIN1))
        goto error;

    loop->opts = enif_thread_opts_create("euv_loop_thread_opts");
    if(loop->opts == NULL) goto error;

    loop->reqs = euv_queue_init();
    if(loop->reqs == NULL) goto error;

    loop->lock = enif_mutex_create("euv_loop_lock");
    if(loop->lock == NULL) goto error;

    loop->cond = enif_cond_create("euv_loop_cond");
    if(loop->cond == NULL) goto error;

    loop->ready = 0;

    // Start up the loop thread
    if(enif_thread_create(
            "euv_loop_thread",
            &loop->tid,
            euv_loop_main,
            loop,
            loop->opts
    ))
        goto error;

    // Wait for thread initialization
    enif_mutex_lock(loop->lock);
    while(loop->ready == 0) {
        enif_cond_wait(loop->cond, loop->lock);
    }
    ready = loop->ready;
    enif_mutex_unlock(loop->lock);

    // If we failed to init, fall through
    // and destroy the loop and notify the
    // caller.
    if(ready == 1)
        return loop;

error:
    if(loop == NULL) return NULL;

    if(loop->opts != NULL) enif_thread_opts_destroy(loop->opts);
    if(loop->reqs != NULL) euv_queue_destroy(loop->reqs);
    if(loop->lock != NULL) enif_mutex_destroy(loop->lock);
    if(loop->cond != NULL) enif_cond_destroy(loop->cond);
    enif_free(loop);

    return NULL;
}


void
euv_loop_destroy(euv_loop_t* loop)
{
    void* resp;

    // Send shutdown message to thread
    enif_thread_join(loop->tid, &resp);

    if(loop->opts != NULL) enif_thread_opts_destroy(loop->opts);
    if(loop->reqs != NULL) euv_queue_destroy(loop->reqs);
    if(loop->lock != NULL) enif_mutex_destroy(loop->lock);
    if(loop->cond != NULL) enif_cond_destroy(loop->cond);
    enif_free(loop);
}

void
euv_async_cb(uv_async_t* handle, int status)
{
    euv_loop_t* loop = (euv_loop_t*) handle->data;
    void* req;

    while(euv_queue_has_item(loop->reqs)) {
        req = euv_queue_pop(loop->reqs);
        // Handle request
    }
}


int
euv_loop_init_int(euv_loop_t* loop)
{
    loop->uvl = uv_loop_new();
    if(loop->uvl == NULL) goto error;
    fprintf(stderr, "MADE LOOP\r\n");

    // This allows us to wakeup the event loop to
    // manage new requests
    loop->wakeup = enif_alloc(sizeof(struct uv_async_s));
    if(loop->wakeup == NULL) goto error;
    fprintf(stderr, "ALLOCED WAKEUP\r\n");

    loop->wakeup->data = (void*) loop;
    if(uv_async_init(loop->uvl, loop->wakeup, euv_async_cb) != 0) goto error;
    fprintf(stderr, "INITED WAKEUP\r\n");

    return 1;

error:
    if(loop->uvl != NULL) uv_loop_delete(loop->uvl);
    return 0;
}


void*
euv_loop_main(void* arg)
{
    euv_loop_t* loop = (euv_loop_t*) arg;
    int ret = euv_loop_init_int(loop);

    fprintf(stderr, "THREAD INITED %d\r\n", ret);

    enif_mutex_lock(loop->lock);
    loop->ready = ret ? 1 : -1;
    enif_cond_signal(loop->cond);
    enif_mutex_unlock(loop->lock);

    if(!ret) return NULL;

    fprintf(stderr, "THREAD STARTING\r\n");

    // Run until we're told to exit
    uv_run(loop->uvl);
    uv_loop_delete(loop->uvl);

    return NULL;
}

