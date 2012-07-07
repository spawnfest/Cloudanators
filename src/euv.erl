% This file is part of euv released under the MIT license.
% See the LICENSE file for more information.

-module(euv).

-export([foo/0]).

-on_load(init/0).

-define(NOT_LOADED, not_loaded(?LINE)).


foo() ->
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
    erlang:load_nif(filename:join(PrivDir, "euv"), 0).


not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
