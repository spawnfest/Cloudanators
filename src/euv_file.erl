% This file is part of euv released under the MIT license.
% See the LICENSE file for more information.

-module(euv_file).
-include("euv_int.hrl").

-export([
    open/2,
    close/1,
    read/2,
    pread/3,
    write/2,
    pwrite/3,
    stat/1,
    lstat/1,
    utime/3
]).

open(Path, Opts) ->
    euv:call([{path, Path}, {opts, Opts}], [{req, ?FS_OPEN}]).

close(Handle) ->
    euv:call(Handle, [], [{req, ?FS_CLOSE}]).

read(Handle, Length) when is_integer(Length), Length >= 0 ->
    euv:call(Handle, [{length, Length}], [{req, ?FS_READ}]).

pread(Handle, Offset, Length) when is_integer(Offset), Offset >= 0,
        is_integer(Length), Length >= 0 ->
    euv:call(Handle, [{length, Length}, {offset, Offset}], [{req, ?FS_READ}]).

write(Handle, Bytes) ->
    euv:call(Handle, [{bytes, Bytes}], [{req, ?FS_WRITE}]).

pwrite(Handle, Offset, Bytes) when is_integer(Offset), Offset >= 0 ->
    euv:call(Handle, [{offset, Offset}, {bytes, Bytes}], [{req, ?FS_WRITE}]).

stat(Path) ->
    euv:call([{path, Path}], [{req, ?FS_STAT}]).

lstat(Path) ->
    euv:call([{path, Path}], [{req, ?FS_LSTAT}]).

utime(Path, ATime, MTime) when is_number(ATime), is_number(MTime) ->
    Args = [
        {path, Path},
        {atime, float(ATime)},
        {mtime, float(MTime)}
    ],
    euv:call(Args, [{req, ?FS_UTIME}]).

