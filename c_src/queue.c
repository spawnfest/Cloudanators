// This file is part of euv released under the MIT license.
// See the LICENSE file for more information.

#include <assert.h>

#include "queue.h"

struct euv_qitem_s
{
    struct euv_qitem_s* next;
    void*               data;
};

typedef struct euv_qitem_s euv_qitem_t;

struct euv_queue_s
{
    ErlNifMutex*        lock;
    euv_qitem_t*        head;
    euv_qitem_t*        tail;
    int                 length;
};

euv_queue_t*
euv_queue_init(const char* name)
{
    euv_queue_t* ret;

    ret = (euv_queue_t*) enif_alloc(sizeof(struct euv_queue_s));
    if(ret == NULL) goto error;

    ret->lock = NULL;
    ret->head = NULL;
    ret->tail = NULL;
    ret->length = 0;

    ret->lock = enif_mutex_create("queue_lock");
    if(ret->lock == NULL) goto error;

    return ret;

error:
    if(ret->lock != NULL) enif_mutex_destroy(ret->lock);
    if(ret != NULL) enif_free(ret);
    return NULL;
}

void
euv_queue_destroy(euv_queue_t* queue)
{
    ErlNifMutex* lock;
    int length;

    enif_mutex_lock(queue->lock);
    lock = queue->lock;
    length = queue->length;

    queue->lock = NULL;
    queue->head = NULL;
    queue->tail = NULL;
    queue->length = -1;
    enif_mutex_unlock(lock);

    assert(length == 0 && "Attempting to destroy a non-empty queue.");
    enif_mutex_destroy(lock);
    enif_free(queue);
}

int
euv_queue_has_item(euv_queue_t* queue)
{
    int ret;

    enif_mutex_lock(queue->lock);
    ret = (queue->head != NULL);
    enif_mutex_unlock(queue->lock);

    return ret;
}

int
euv_queue_push(euv_queue_t* queue, void* item)
{
    euv_qitem_t* entry = (euv_qitem_t*) enif_alloc(sizeof(struct euv_qitem_s));
    if(entry == NULL) return 0;

    entry->data = item;
    entry->next = NULL;

    enif_mutex_lock(queue->lock);

    assert(queue->length >= 0 && "Invalid queue size at push");

    if(queue->tail != NULL) {
        queue->tail->next = entry;
    }

    queue->tail = entry;

    if(queue->head == NULL) {
        queue->head = queue->tail;
    }

    queue->length += 1;

    enif_mutex_unlock(queue->lock);

    return 1;
}

void*
euv_queue_pop(euv_queue_t* queue)
{
    euv_qitem_t* entry;
    void* item;

    enif_mutex_lock(queue->lock);

    assert(queue->length >= 0 && "Invalid queue size at pop.");

    if(queue->length == 0) {
        enif_mutex_unlock(queue->lock);
        return NULL;
    }

    // Remove the entry and return the payload.

    entry = queue->head;
    queue->head = entry->next;
    entry->next = NULL;

    if(queue->head == NULL) {
        assert(queue->tail == entry && "Invalid queue state: Bad tail.");
        queue->tail = NULL;
    }

    queue->length -= 1;

    enif_mutex_unlock(queue->lock);

    item = entry->data;
    enif_free(entry);

    return item;
}
