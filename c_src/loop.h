// This file is part of euv released under the MIT license.
// See the LICENSE file for more information.

#ifndef EUV_LOOP_H
#define EUV_LOOP_H


#include "erl_nif.h"


typedef struct euv_loop_s euv_loop_t;

typedef enum {
    euv_close = 0,

    euv_file_open,
    euv_file_close,
    euv_file_read,
    euv_file_write,

    euv_req_type_err
} euv_req_type;

typedef struct {
    ErlNifPid       pid;
    euv_loop_t*     loop;
    void*           data;
} euv_handle_t;

typedef struct {
    euv_req_type    req_type;
    ErlNifEnv*      env;
    ErlNifPid       pid;
    ERL_NIF_TERM    ref;
    ERL_NIF_TERM    args;
    euv_handle_t*   handle;
} euv_req_t;


void euv_handle_destroy(ErlNifEnv* env, void* obj);


euv_req_t*  euv_req_init();
void euv_req_destroy(euv_req_t* req);


euv_loop_t* euv_loop_init();
void euv_loop_destroy(euv_loop_t* loop);

int euv_loop_is(euv_loop_t* loop, ErlNifEnv* env, ERL_NIF_TERM name);
int euv_loop_notify(euv_loop_t* loop);
int euv_loop_queue(euv_loop_t* loop, euv_req_t* req);


#endif // Included loop.h
