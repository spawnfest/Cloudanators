// This file is part of euv released under the MIT license.
// See the LICENSE file for more information.

#ifndef EUV_FS_H
#define EUV_FS_H


#include "erl_nif.h"

#include "loop.h"


typedef struct euv_fs_s euv_fs_t;

void euv_fs_dtor(void* obj);


void euv_fs_stat(euv_loop_t* loop, euv_req_t* req);
void euv_fs_lstat(euv_loop_t* loop, euv_req_t* req);
void euv_fs_utime(euv_loop_t* loop, euv_req_t* req);



#endif // Included fs.h
