% This file is part of euv released under the MIT license.
% See the LICENSE file for more information.

-module(euv).

-export([
    call/2,
    call/3,
    submit/3,
    submit/4
]).

-on_load(init/0).

-define(NOT_LOADED, not_loaded(?LINE)).


call(Args, Opts) ->
    Ref = make_ref(),
    ok = submit(Ref, Args, Opts),
    receive {Ref, Resp} -> Resp end.


call(Handle, Args, Opts) ->
    Ref = make_ref(),
    ok = submit(Ref, Handle, Args, Opts),
    receive {Ref, Resp} -> Resp end.


submit(_Ref, _Args, _Opts) ->
    ?NOT_LOADED.


submit(_Ref, _Handle, _Args, _Opts) ->
    ?NOT_LOADED.


init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    Opts = case application:get_all_env(?MODULE) of
        [] -> [{loops, [default]}];
        Else -> Else
    end,
    erlang:load_nif(filename:join(PrivDir, "euv"), Opts).


not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
