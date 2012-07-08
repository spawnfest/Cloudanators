% This file is part of euv released under the MIT license.
% See the LICENSE file for more information.

-module(euv_file).
-include("euv_int.hrl").

-export([
    open/2,
    close/1,
    stat/1,
    lstat/1,
    utime/3
]).

open(Path, Opts) ->
    euv:call([{path, Path}, {opts, Opts}], [{req, ?FS_OPEN}]).

close(Handle) ->
    euv:call(Handle, [], [{req, ?FS_CLOSE}]).

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

