// This file is part of euv released under the MIT license.
// See the LICENSE file for more information.

#ifndef EUV_LOOP_H
#define EUV_LOOP_H


#include "erl_nif.h"
#include "uv.h"

typedef struct euv_loop_s euv_loop_t;
typedef void (*handle_dtor_t) (euv_loop_t*, void*);

typedef enum {
    EUV_REQ_UNKNOWN = 0,

    EUV_REQ_FS_OPEN,
    EUV_REQ_FS_CLOSE,
    EUV_REQ_FS_STAT,
    EUV_REQ_FS_LSTAT,
    EUV_REQ_FS_UTIME,

    EUV_REQ_MAX
} euv_req_type;

typedef struct {
    ErlNifPid           pid;
    euv_loop_t*         loop;
    void*               data;
    handle_dtor_t       dtor;
} euv_handle_t;

typedef struct {
    euv_req_type    type;
    ErlNifEnv*      env;
    ErlNifPid       pid;
    ERL_NIF_TERM    ref;
    ERL_NIF_TERM    args;
    euv_handle_t*   handle;
} euv_req_t;


euv_handle_t* euv_handle_init(euv_loop_t* loop, euv_req_t* req);
void euv_handle_destroy(ErlNifEnv* env, void* obj);


euv_req_t*  euv_req_init();
void euv_req_destroy(euv_req_t* req);

euv_req_type euv_req_get_type(ErlNifEnv* env, ERL_NIF_TERM opts);
void euv_req_resp(euv_req_t* req, ERL_NIF_TERM resp);
void euv_req_resp_ok(euv_req_t* req, ERL_NIF_TERM val);
void euv_req_resp_error(euv_req_t* req, ERL_NIF_TERM val);
ERL_NIF_TERM euv_req_uv_error(euv_req_t* req);
ERL_NIF_TERM euv_req_errno(euv_req_t* req, int errno);

euv_loop_t* euv_loop_init(ErlNifEnv* env, ERL_NIF_TERM name);
void euv_loop_destroy(euv_loop_t* loop);

uv_loop_t* euv_loop_uvl(euv_loop_t* loop);
int euv_loop_is(euv_loop_t* loop, ErlNifEnv* env, ERL_NIF_TERM name);
int euv_loop_notify(euv_loop_t* loop);
int euv_loop_queue(euv_loop_t* loop, euv_req_t* req);


#endif // Included loop.h
