// This file is part of euv released under the MIT license.
// See the LICENSE file for more information.

#ifndef EUV_QUEUE_H
#define EUV_QUEUE_H

#include "erl_nif.h"

typedef struct euv_queue_s euv_queue_t;

euv_queue_t* euv_queue_init();
void euv_queue_destroy(euv_queue_t* queue);

int euv_queue_has_item(euv_queue_t* queue);

int euv_queue_push(euv_queue_t* queue, void* item);
void* euv_queue_pop(euv_queue_t* queue);

#endif // Included queue.h
