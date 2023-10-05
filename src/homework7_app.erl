%%%-------------------------------------------------------------------
%% @doc homework7 public API
%% @end
%%%-------------------------------------------------------------------

-module(homework7_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Pid = my_cache:start_link(),
    Dispatch = cowboy_router:compile([
        {'_',[
            {"/api/cache_server", homework7_h, [{pid, Pid}]}
        ]}
    ]),
    {ok, _Pid} = cowboy:start_clear(http, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    homework7_sup:start_link().

stop(_State) ->
    my_cache:stop(),
    ok = cowboy:stop_listener(http).
