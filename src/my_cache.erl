-module(my_cache).

-behavior(gen_server).

-export([start_link/0, create/1, insert/2, insert/3, lookup/1, delete/1, stop/0, start/0]).

-export([init/1, handle_call/3, terminate/2, code_change/3, handle_info/2, handle_cast/2]).

-define(SERVER, ?MODULE).
-define(TABLE, nikiforov).

start_link() ->
	{ok, Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
	create(?TABLE),
	Pid.

start() ->
	application:ensure_all_started(?MODULE),
	{ok, Pid} = gen_server:start(?MODULE, [], []),
	create(?TABLE),
	{ok, Pid}.
%%	application:ensure_all_started(?MODULE).

create(TableName) ->
	gen_server:call(?MODULE, {create, TableName}).

insert(Key, Value) ->
	gen_server:call(?MODULE, {insert, Key, Value}).

insert(Key, Value, Ttl) ->
	gen_server:call(?MODULE, {insert, Key, Value, Ttl}).

lookup(Key) ->
	gen_server:call(?MODULE, {lookup, Key}).

delete(Key) ->
	gen_server:call(?MODULE, {delete, Key}).

stop() ->
	gen_server:stop(?MODULE).

init([]) ->
	{ok, []}.

handle_call({create, TableName}, _From, State) ->
	Table = ets:new(TableName, [public, ordered_set, named_table]),
	erlang:send_after(60000, self(), {delete_obsolete, Table}),
	{reply, Table, State};
handle_call({insert, Key, Value, Ttl}, _From, State) ->
	ExpireTime = calendar:datetime_to_gregorian_seconds(calendar:local_time()) + Ttl,
	ets:insert(?TABLE, {Key, Value, {expire_time, ExpireTime}}),
	{reply, ok, State};
handle_call({insert, Key, Value}, _From, State) ->
	CurrentUnixtime = get_unixtime(),
	ets:insert(?TABLE, {Key, Value, {timestamp, CurrentUnixtime}}),
	{reply, ok, State};
handle_call({lookup, Key}, _From, State) ->
	CurrentUnixTime = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	case ets:lookup(?TABLE, Key) of
		{Key, Value, {timestamp, _}} -> {reply, Value, State};
		[{Key, Value, {timestamp, _}}] -> {reply, Value, State};
		[{Key, Value, {expire_time, ExpireTime}}] when CurrentUnixTime =< ExpireTime ->
			{reply, Value, State};
		[{Key, _Value, {expire_time, _ExpireTime}}] ->
			ets:delete(?TABLE, Key),
			{reply, undefined, State};
		_ -> {reply, undefined, State}
	end;
handle_call({delete, Key}, _From, State) ->
	ets:delete(?TABLE, Key),
	{reply, ok, State};
handle_call({lookup_by_date, DateFrom, DateTo}, _From, State) ->
	DateFromUnixtime = get_unixtime(DateFrom),
	DateToUnixtime = get_unixtime(DateTo),
	Values = lookup_by_date(ets:first(?TABLE), DateFromUnixtime, DateToUnixtime, []),
	{reply, Values, State};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({delete_obsolete, TableName}, State) ->
	delete_obsolete(TableName),
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

code_change(_OldVersion, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	normal.

delete_obsolete(TableName) ->
	CurrentUnixtime = get_unixtime(),
	FirstKey = ets:first(TableName),
	ok = delete_obsolete(TableName, FirstKey, CurrentUnixtime).

delete_obsolete(_TableName, '$end_of_table', _Unixtime) ->
	io:format(delete),
	erlang:send_after(60000, self(), {delete_obsolete, ?TABLE}),
	ok;
delete_obsolete(TableName, Key, Unixtime) ->
	case ets:lookup(TableName, Key) of
		[{Key, _Value, {expire_time, EndDateSec}}] when Unixtime >= EndDateSec ->
			ets:delete(TableName, Key);
		_ -> true
	end,
	delete_obsolete(TableName, ets:next(TableName, Key), Unixtime).

lookup_by_date('$end_of_table', _TimeFrom, _TimeTo, Acc) ->
	lists:reverse(Acc);
lookup_by_date(Key, TimeFrom, TimeTo, Acc) ->
	case ets:lookup(?TABLE, Key) of
		[{K, V, {expire_time, ExpireTime}}] when ExpireTime >= TimeFrom, ExpireTime =< TimeTo ->
			lookup_by_date(ets:next(?TABLE, Key), TimeFrom, TimeTo, [{K, V} | Acc]);
		[{K, V, {timestamp, Time}}] when Time >= TimeFrom, Time =< TimeTo ->
			lookup_by_date(ets:next(?TABLE, Key), TimeFrom, TimeTo, [{K, V} | Acc]);
		_ -> lookup_by_date(ets:next(?TABLE, Key), TimeFrom, TimeTo, Acc)
	end.

get_unixtime() ->
	calendar:datetime_to_gregorian_seconds(calendar:local_time()).

get_unixtime(DateTimeBin) ->
	[Date, Time] = binary:split(DateTimeBin, <<" ">>),
	[YearBin, MonthBin, DayBin] = binary:split(Date, <<"/">>, [global]),
	[HourBin, MinBin, SecBin] = binary:split(Time, <<":">>, [global]),
	GregorianTime = {
		{binary_to_integer(YearBin), binary_to_integer(MonthBin), binary_to_integer(DayBin)},
		{binary_to_integer(HourBin), binary_to_integer(MinBin), binary_to_integer(SecBin)}
	},
	calendar:datetime_to_gregorian_seconds(GregorianTime).
