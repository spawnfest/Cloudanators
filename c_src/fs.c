// This file is part of euv released under the MIT license.
// See the LICENSE file for more information.

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "uv.h"

#include "euv.h"
#include "fs.h"
#include "util.h"


struct euv_fs_s {
    uv_fs_t         fsreq;
    uv_file         fd;
    ErlNifBinary    buf;
};


static euv_fs_t*
_init_fs_handle(euv_loop_t* loop, euv_req_t* req)
{
    euv_handle_t* handle = euv_handle_init(loop, req);
    euv_fs_t* data;

    if(handle == NULL)
        return NULL;

    assert(req->handle == NULL && "handle already set");
    req->handle = handle;

    data = (euv_fs_t*) enif_alloc(sizeof(euv_fs_t));
    if(data == NULL)
        return NULL;
    memset(data, 0, sizeof(euv_fs_t));

    data->fsreq.data = req;
    data->fd = 0;
    data->buf.data = NULL;
    handle->data = data;
    handle->dtor = euv_fs_dtor;

    return data;
}


static euv_fs_t*
_get_fs_handle(euv_req_t* req)
{
    euv_fs_t* data;
    assert(req->handle != NULL && "invalid request handle");
    assert(req->handle->data != NULL && "invalid requeset handle data");
    data = (euv_fs_t*) req->handle->data;
    data->fsreq.data = req;
    return data;
}


static char*
_get_path(euv_req_t* req)
{
    ErlNifBinary bin;
    ERL_NIF_TERM pterm;
    char* ret;

    if(!euv_pl_lookup(req->env, req->args, EUV_ATOM_PATH, &pterm))
        return NULL;

    if(!enif_inspect_iolist_as_binary(req->env, pterm, &bin))
        return NULL;

    ret = enif_alloc(bin.size + 1);
    if(ret == NULL)
        return NULL;

    memcpy(ret, bin.data, bin.size);
    ret[bin.size] = 0;

    return ret;
}


static ERL_NIF_TERM
_kv_add(euv_req_t* req, ERL_NIF_TERM k, int64_t v, ERL_NIF_TERM l)
{
    ERL_NIF_TERM ival = enif_make_int64(req->env, v);
    ERL_NIF_TERM item = enif_make_tuple2(req->env, k, ival);
    return euv_list_append(req->env, item, l);
}


void
euv_fs_dtor(euv_loop_t* loop, void* obj)
{
    euv_fs_t* data = (euv_fs_t*) obj;
    if(data->fd > 0)
        uv_fs_close(euv_loop_uvl(loop), &data->fsreq, data->fd, NULL);
    uv_fs_req_cleanup(&data->fsreq);
    if(data->buf.data != NULL)
        enif_release_binary(&data->buf);
    enif_free(data);
}


int
euv_fs_open_flags(euv_req_t* req)
{
    ERL_NIF_TERM opts;
    ERL_NIF_TERM opt;
    int ret = 0;

    if(!euv_pl_lookup(req->env, req->args, EUV_ATOM_OPTS, &opts))
        return ret;

    while(enif_get_list_cell(req->env, opts, &opt, &opts)) {
        if(enif_compare(opt, EUV_ATOM_READ) == 0)
            ret |= O_RDONLY;
        else if(enif_compare(opt, EUV_ATOM_WRITE) == 0)
            ret |= O_WRONLY;
        else if(enif_compare(opt, EUV_ATOM_APPEND) == 0)
            ret |= O_APPEND;
        else if(enif_compare(opt, EUV_ATOM_EXCLUSIVE) == 0)
            ret |= O_EXCL;
    }

    if(ret == 0)
        return O_RDONLY;

    if((ret & O_WRONLY) && !(ret & O_RDONLY))
        ret |= O_TRUNC;

    return ret;
}


void
euv_fs_open_cb(uv_fs_t* fsreq)
{
    euv_req_t* req = (euv_req_t*) fsreq->data;
    euv_fs_t* data = (euv_fs_t*) req->handle->data;
    ERL_NIF_TERM resp;

    if(fsreq->result > 0) {
        data->fd = (uv_file) fsreq->result;
        //enif_keep_resource(req->handle);
        resp = enif_make_resource(req->env, req->handle);
        resp = enif_make_tuple2(req->env, EUV_ATOM_EUVFILE, resp);
        resp = euv_make_ok(req->env, resp);
    } else {
        resp = euv_req_errno(req, fsreq->errorno);
    }

    uv_fs_req_cleanup(fsreq);
    euv_req_resp(req, resp);
}


void
euv_fs_open(euv_loop_t* loop, euv_req_t* req)
{
    euv_fs_t* data = _init_fs_handle(loop, req);
    int flags = euv_fs_open_flags(req);
    int mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH;
    char* path = NULL;

    if(data == NULL) {
        euv_req_resp_error(req, EUV_ATOM_INTERNAL_ERROR);
        goto done;
    }

    path = _get_path(req);
    if(path == NULL) {
        euv_req_resp_error(req, EUV_ATOM_INVALID_REQ);
        goto done;
    }

    if(uv_fs_open(
                euv_loop_uvl(loop),
                &data->fsreq,
                path,
                flags,
                mode,
                euv_fs_open_cb
            ) != 0)
        euv_req_resp_error(req, EUV_ATOM_INTERNAL_ERROR);

done:
    if(path != NULL) enif_free(path);
    return;
}


void
euv_fs_close_cb(uv_fs_t* fsreq)
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
euv_fs_close(euv_loop_t* loop, euv_req_t* req)
{
    euv_fs_t* data = _get_fs_handle(req);
    if(uv_fs_close(
                euv_loop_uvl(loop),
                &data->fsreq,
                data->fd,
                euv_fs_close_cb
            ) != 0)
        euv_req_resp_error(req, EUV_ATOM_INTERNAL_ERROR);
}


void
euv_fs_read_cb(uv_fs_t* fsreq)
{
    euv_req_t* req = (euv_req_t*) fsreq->data;
    euv_fs_t* data = (euv_fs_t*) req->handle->data;
    ERL_NIF_TERM resp;

    if(fsreq->result == 0) {
        resp = EUV_ATOM_EOF;
    } else if(fsreq->result > 0) {
        if(!enif_realloc_binary(&data->buf, fsreq->result)) {
            enif_release_binary(&data->buf);
            resp = euv_make_error(req->env, EUV_ATOM_NOMEM);
        } else {
            resp = enif_make_binary(req->env, &data->buf);
            resp = euv_make_ok(req->env, resp);
        }
    } else {
        enif_release_binary(&data->buf);
        resp = euv_req_errno(req, fsreq->errorno);
    }
    data->buf.data = NULL;

    uv_fs_req_cleanup(fsreq);
    euv_req_resp(req, resp);
}


void
euv_fs_read(euv_loop_t* loop, euv_req_t* req)
{
    euv_fs_t* data = _get_fs_handle(req);
    ERL_NIF_TERM opt;
    size_t len = 0;
    int64_t offset;

    assert(data->buf.data == NULL && "stale buffer allocated");

    if(!euv_pl_lookup(req->env, req->args, EUV_ATOM_LENGTH, &opt)) {
        euv_req_resp_error(req, EUV_ATOM_INVALID_REQ);
        return;
    }

    if(!enif_get_uint64(req->env, opt, &len)) {
        euv_req_resp_error(req, EUV_ATOM_BADARG);
        return;
    }

    if(!euv_pl_lookup(req->env, req->args, EUV_ATOM_OFFSET, &opt)) {
        offset = -1;
    } else if(!enif_get_int64(req->env, opt, (ErlNifSInt64*) &offset)) {
        euv_req_resp_error(req, EUV_ATOM_BADARG);
        return;
    }

    if(!enif_alloc_binary(len, &data->buf)) {
        euv_req_resp_error(req, EUV_ATOM_NOMEM);
        return;
    }

    if(uv_fs_read(
                euv_loop_uvl(loop),
                &data->fsreq,
                data->fd,
                data->buf.data,
                data->buf.size,
                offset,
                euv_fs_read_cb
            ) != 0)
        euv_req_resp_error(req, EUV_ATOM_INTERNAL_ERROR);
}


void
euv_fs_write_cb(uv_fs_t* fsreq)
{

}


void
euv_fs_write(euv_loop_t* loop, euv_req_t* req)
{

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
    euv_fs_t* data = _init_fs_handle(loop, req);
    char* path = NULL;

    if(data == NULL) {
        euv_req_resp_error(req, EUV_ATOM_INTERNAL_ERROR);
        goto done;
    }

    path = _get_path(req);
    if(path == NULL) {
        euv_req_resp_error(req, EUV_ATOM_INVALID_REQ);
        goto done;
    }

    if(uv_fs_stat(euv_loop_uvl(loop), &data->fsreq, path, euv_fs_stat_cb) != 0)
        euv_req_resp_error(req, EUV_ATOM_INTERNAL_ERROR);

done:
    if(path != NULL) enif_free(path);
}


void
euv_fs_lstat(euv_loop_t* loop, euv_req_t* req)
{
    euv_fs_t* data = _init_fs_handle(loop, req);
    char* path = NULL;

    if(data == NULL) {
        euv_req_resp_error(req, EUV_ATOM_INTERNAL_ERROR);
        goto done;
    }

    path = _get_path(req);
    if(path == NULL) {
        euv_req_resp_error(req, EUV_ATOM_INVALID_REQ);
        goto done;
    }

    if(uv_fs_lstat(euv_loop_uvl(loop), &data->fsreq, path, euv_fs_stat_cb) != 0)
        euv_req_resp_error(req, EUV_ATOM_INTERNAL_ERROR);

done:
    if(path != NULL) enif_free(path);
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
    euv_fs_t* data = _init_fs_handle(loop, req);
    ERL_NIF_TERM opt;
    char* path = NULL;
    double atime;
    double mtime;

    if(data == NULL) {
        euv_req_resp_error(req, EUV_ATOM_INTERNAL_ERROR);
        goto done;
    }

    path = _get_path(req);
    if(path == NULL) {
        euv_req_resp_error(req, EUV_ATOM_INVALID_REQ);
        goto done;
    }

    if(!euv_pl_lookup(req->env, req->args, EUV_ATOM_ATIME, &opt)) {
        euv_req_resp_error(req, EUV_ATOM_INVALID_REQ);
        goto done;
    }

    if(!enif_get_double(req->env, opt, &atime)) {
        euv_req_resp_error(req, EUV_ATOM_BADARG);
        goto done;
    }

    if(!euv_pl_lookup(req->env, req->args, EUV_ATOM_MTIME, &opt)) {
        euv_req_resp_error(req, EUV_ATOM_INVALID_REQ);
        goto done;
    }

    if(!enif_get_double(req->env, opt, &mtime)) {
        euv_req_resp_error(req, EUV_ATOM_BADARG);
        goto done;
    }

    if(uv_fs_utime(
                euv_loop_uvl(loop),
                &data->fsreq,
                path,
                atime,
                mtime,
                euv_fs_utime_cb
            ) != 0)
        euv_req_resp_error(req, EUV_ATOM_INTERNAL_ERROR);

done:
    if(path != NULL) enif_free(path);
}
