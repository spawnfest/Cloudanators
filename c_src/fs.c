// This file is part of euv released under the MIT license.
// See the LICENSE file for more information.

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "uv.h"

#include "euv.h"
#include "fs.h"
#include "util.h"


static int
_init_fs_handle(euv_loop_t* loop, euv_req_t* req)
{
    euv_handle_t* h = euv_handle_init(loop, req);
    uv_fs_t* d;

    if(h == NULL)
        return 0;

    req->handle = h;

    d = (uv_fs_t*) enif_alloc(sizeof(uv_fs_t));
    if(d == NULL)
        return 0;

    d->data = req;
    h->data = d;
    h->dtor = euv_fs_dtor;

    return 1;
}


static const char*
_get_path(euv_req_t* req, ErlNifBinary* bin)
{
    ERL_NIF_TERM pterm;

    if(!euv_pl_lookup(req->env, req->args, EUV_ATOM_PATH, &pterm))
        return NULL;

    if(!enif_inspect_iolist_as_binary(req->env, pterm, bin))
        return NULL;

    return (const char*) bin->data;
}


static ERL_NIF_TERM
_kv_add(euv_req_t* req, ERL_NIF_TERM k, int64_t v, ERL_NIF_TERM l)
{
    ERL_NIF_TERM ival = enif_make_int64(req->env, v);
    ERL_NIF_TERM item = enif_make_tuple2(req->env, k, ival);
    return euv_list_append(req->env, item, l);
}


void
euv_fs_dtor(void* obj)
{
    uv_fs_t* req = (uv_fs_t*) obj;
    enif_free(req);
}


void
euv_fs_stat_cb(uv_fs_t* fsreq)
{
    euv_req_t* req = (euv_req_t*) fsreq->data;
    ERL_NIF_TERM resp = enif_make_list(req->env, 0);
    struct stat* s;

    if(fsreq->result != 0) {
        resp = euv_req_errno(req, fsreq->errorno);
    } else {
        s = fsreq->ptr;
        assert(s != NULL && "fs_stat failed");
        resp = _kv_add(req, EUV_ATOM_DEV, s->st_dev, resp);
        resp = _kv_add(req, EUV_ATOM_INO, s->st_ino, resp);
        resp = _kv_add(req, EUV_ATOM_MODE, s->st_mode, resp);
        resp = _kv_add(req, EUV_ATOM_NLINK, s->st_nlink, resp);
        resp = _kv_add(req, EUV_ATOM_UID, s->st_uid, resp);
        resp = _kv_add(req, EUV_ATOM_GID, s->st_gid, resp);
        resp = _kv_add(req, EUV_ATOM_RDEV, s->st_rdev, resp);
#if _WIN32
        resp = _kv_add(req, EUV_ATOM_ATIME, s->st_atime, resp);
        resp = _kv_add(req, EUV_ATOM_MTIME, s->st_mtime, resp);
        resp = _kv_add(req, EUV_ATOM_CTIME, s->st_ctime, resp);
#elif !defined(_POSIX_C_SOURCE) || defined(_DARWIN_C_SOURCE)
        resp = _kv_add(req, EUV_ATOM_ATIME, s->st_atimespec.tv_sec, resp);
        resp = _kv_add(req, EUV_ATOM_MTIME, s->st_mtimespec.tv_sec, resp);
        resp = _kv_add(req, EUV_ATOM_CTIME, s->st_ctimespec.tv_sec, resp);
#else
        resp = _kv_add(req, EUV_ATOM_ATIME, s->st_atim.tv_sec, resp);
        resp = _kv_add(req, EUV_ATOM_MTIME, s->st_ctim.tv_sec, resp);
        resp = _kv_add(req, EUV_ATOM_CTIME, s->st_mtim.tv_sec, resp);
#endif
        resp = _kv_add(req, EUV_ATOM_SIZE, s->st_size, resp);
        resp = _kv_add(req, EUV_ATOM_BLOCKS, s->st_blocks, resp);
        resp = _kv_add(req, EUV_ATOM_BLKSIZE, s->st_blksize, resp);
        resp = _kv_add(req, EUV_ATOM_FLAGS, s->st_flags, resp);
        resp = _kv_add(req, EUV_ATOM_GEN, s->st_gen, resp);
        resp = euv_make_ok(req->env, resp);
    }

    uv_fs_req_cleanup(fsreq);
    euv_req_resp(req, resp);
}


void
euv_fs_stat(euv_loop_t* loop, euv_req_t* req)
{
    ErlNifBinary bin;
    const char* path;

    if(!_init_fs_handle(loop, req)) {
        euv_req_resp_error(req, EUV_ATOM_INTERNAL_ERROR);
        return;
    }

    path = _get_path(req, &bin);
    if(path == NULL) {
        euv_req_resp_error(req, EUV_ATOM_INVALID_REQ);
        return;
    }

    if(uv_fs_stat(euv_loop_uvl(loop),
                req->handle->data, path, euv_fs_stat_cb) != 0)
        euv_req_resp_error(req, EUV_ATOM_INTERNAL_ERROR);
}


void
euv_fs_lstat(euv_loop_t* loop, euv_req_t* req)
{
    ErlNifBinary bin;
    const char* path;

    if(!_init_fs_handle(loop, req)) {
        euv_req_resp_error(req, EUV_ATOM_INTERNAL_ERROR);
        return;
    }

    path = _get_path(req, &bin);
    if(path == NULL) {
        euv_req_resp_error(req, EUV_ATOM_INVALID_REQ);
        return;
    }

    if(uv_fs_lstat(euv_loop_uvl(loop),
                req->handle->data, path, euv_fs_stat_cb) != 0)
        euv_req_resp_error(req, EUV_ATOM_INTERNAL_ERROR);
}


void
euv_fs_utime_cb(uv_fs_t* fsreq)
{
    euv_req_t* req = (euv_req_t*) fsreq->data;
    ERL_NIF_TERM resp;

    if(fsreq->result == 0) {
        resp = EUV_ATOM_OK;
    } else {
        resp = euv_req_errno(req, fsreq->errorno);
    }

    uv_fs_req_cleanup(fsreq);
    euv_req_resp(req, resp);
}


void
euv_fs_utime(euv_loop_t* loop, euv_req_t* req)
{
    ERL_NIF_TERM opt;
    ErlNifBinary bin;
    const char* path;
    double atime;
    double mtime;

    if(!_init_fs_handle(loop, req)) {
        euv_req_resp_error(req, EUV_ATOM_INTERNAL_ERROR);
        return;
    }

    path = _get_path(req, &bin);
    if(path == NULL) {
        euv_req_resp_error(req, EUV_ATOM_INVALID_REQ);
        return;
    }

    if(!euv_pl_lookup(req->env, req->args, EUV_ATOM_ATIME, &opt)) {
        euv_req_resp_error(req, EUV_ATOM_INVALID_REQ);
        return;
    }

    if(!enif_get_double(req->env, opt, &atime)) {
        euv_req_resp_error(req, EUV_ATOM_BADARG);
        return;
    }

    if(!euv_pl_lookup(req->env, req->args, EUV_ATOM_MTIME, &opt)) {
        euv_req_resp_error(req, EUV_ATOM_INVALID_REQ);
        return;
    }

    if(!enif_get_double(req->env, opt, &mtime)) {
        euv_req_resp_error(req, EUV_ATOM_BADARG);
        return;
    }

    if(uv_fs_utime(euv_loop_uvl(loop),
                req->handle->data, path, atime, mtime, euv_fs_utime_cb) != 0)
        euv_req_resp_error(req, EUV_ATOM_INTERNAL_ERROR);
}
