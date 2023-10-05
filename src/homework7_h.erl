-module(homework7_h).

-export([init/2]).

-define(KEY, <<"key">>).
-define(VALUE, <<"value">>).
-define(TTL, <<"ttl">>).
-define(DATE_FROM, <<"date_from">>).
-define(DATE_TO, <<"date_to">>).

init(#{method := <<"POST">>} = Req0, Opts) ->
	{ok, Body, _Req} = cowboy_req:read_body(Req0),
	Pid = proplists:get_value(pid, Opts),
	io:format("init"),
	Res = request_handler(jsx:decode(Body, [return_maps]), Pid),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain">>
	}, jsx:encode(Res), Req0),
	{ok, Req, Opts}.

request_handler(#{<<"action">> := <<"insert">>} = Data, Pid) ->
	Key = maps:get(?KEY, Data),
	Value = maps:get(?VALUE, Data),
	case maps:is_key(?TTL, Data) of
		true ->
			Ttl = maps:get(?TTL, Data),
			ok = gen_server:call(Pid, {insert, Key, Value, Ttl});
		false ->
			ok = gen_server:call(Pid, {insert, Key, Value})
	end,
	#{<<"result">> => <<"ok">>};
request_handler(#{<<"action">> := <<"lookup">>} = Data, Pid) ->
	Key = maps:get(?KEY, Data),
	Value = gen_server:call(Pid, {lookup, Key}),
	#{<<"result">> => Value};
request_handler(#{<<"action">> := <<"lookup_by_date">>} = Data, Pid) ->
	DateFrom = maps:get(?DATE_FROM, Data),
	io:format("~p~n", [DateFrom]),
	DateTo = maps:get(?DATE_TO, Data),
	Res = gen_server:call(Pid, {lookup_by_date, DateFrom, DateTo}),
	#{<<"result">> => Res}.
