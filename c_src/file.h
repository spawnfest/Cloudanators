// This file is part of euv released under the MIT license.
// See the LICENSE file for more information.

#ifndef EUV_FILE_H
#define EUV_FILE_H


#include "erl_nif.h"

#include "loop.h"


typedef struct euv_file_s euv_file_t;


euv_file_t* euv_file_init(ErlNifEnv* env, ERL_NIF_TERM path, ERL_NIF_TERM opts);
void euv_file_dtor(ErlNifEnv* env, void* obj);

int euv_file_open(euv_file_t* file);
int euv_file_close(euv_file_t* file);


#endif // Included file.h
