// This file is part of euv released under the MIT license.
// See the LICENSE file for more information.

#ifndef EUV_LOOP_H
#define EUV_LOOP_H

#include "erl_nif.h"

typedef struct euv_loop_s euv_loop_t;

euv_loop_t* euv_loop_init();
void euv_loop_destroy(euv_loop_t* loop);

#endif // Included queue.h
