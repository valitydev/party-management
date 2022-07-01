-module(pm_datetime).

%%

-export([format_now/0]).
-export([compare/2]).
-export([between/2]).
-export([between/3]).
-export([add_interval/2]).

-include_lib("damsel/include/dmsl_base_thrift.hrl").

-type unix_timestamp() :: integer().
-type timestamp() :: dmsl_base_thrift:'Timestamp'().
-type timestamp_interval() :: dmsl_base_thrift:'TimestampInterval'().
-type timestamp_interval_bound() :: dmsl_base_thrift:'TimestampIntervalBound'().

%% not exported from calendar module
-type rfc3339_time_unit() ::
    microsecond
    | millisecond
    | nanosecond
    | second.

-export_type([timestamp/0]).

%%

-spec format_ts(unix_timestamp()) -> timestamp().
format_ts(Ts) when is_integer(Ts) ->
    format_ts(Ts, second).

-spec format_now() -> timestamp().
format_now() ->
    USec = erlang:system_time(microsecond),
    format_ts(USec, microsecond).

-spec compare(timestamp(), timestamp()) -> later | earlier | simultaneously.
compare(T1, T2) when is_binary(T1) andalso is_binary(T2) ->
    compare_int(to_integer(T1), to_integer(T2)).

% Compare inclusivly! undefined == ∞
-spec between(timestamp(), timestamp() | undefined, timestamp() | undefined) -> boolean().
between(Timestamp, Start, End) ->
    LB = to_interval_bound(Start, inclusive),
    UB = to_interval_bound(End, inclusive),
    between(Timestamp, #base_TimestampInterval{lower_bound = LB, upper_bound = UB}).

-spec between(timestamp(), timestamp_interval()) -> boolean().
between(Timestamp, #base_TimestampInterval{lower_bound = LB, upper_bound = UB}) ->
    check_bound(Timestamp, LB, later) andalso
        check_bound(Timestamp, UB, earlier).

-spec add_interval(timestamp(), {Years, Months, Days}) -> timestamp() when
    Years :: integer() | undefined,
    Months :: integer() | undefined,
    Days :: integer() | undefined.
add_interval(Timestamp, {YY, MM, DD}) ->
    TSSeconds = erlang:convert_time_unit(to_integer(Timestamp), microsecond, second),
    {Date, Time} = genlib_time:unixtime_to_daytime(TSSeconds),
    NewDate = genlib_time:shift_date(Date, {nvl(YY), nvl(MM), nvl(DD)}),
    format_ts(genlib_time:daytime_to_unixtime({NewDate, Time})).

-spec parse(binary(), rfc3339_time_unit()) -> integer().
parse(Bin, Precision) when is_binary(Bin) ->
    Str = erlang:binary_to_list(Bin),
    calendar:rfc3339_to_system_time(Str, [{unit, Precision}]).

%% Internal functions

-spec format_ts(integer(), rfc3339_time_unit()) -> timestamp().
format_ts(Ts, Unit) ->
    Str = calendar:system_time_to_rfc3339(Ts, [{unit, Unit}, {offset, "Z"}]),
    erlang:list_to_binary(Str).

-spec to_integer(timestamp()) -> integer().
to_integer(Timestamp) ->
    parse(Timestamp, microsecond).

to_interval_bound(undefined, _) ->
    undefined;
to_interval_bound(Timestamp, BoundType) ->
    #base_TimestampIntervalBound{bound_type = BoundType, bound_time = Timestamp}.

compare_int(T1, T2) ->
    case T1 > T2 of
        true ->
            later;
        false when T1 < T2 ->
            earlier;
        false when T1 =:= T2 ->
            simultaneously
    end.

-spec check_bound(timestamp(), timestamp_interval_bound(), later | earlier) -> boolean().
check_bound(_, undefined, _) ->
    true;
check_bound(Timestamp, #base_TimestampIntervalBound{bound_type = Type, bound_time = BoundTime}, Operator) ->
    case compare(Timestamp, BoundTime) of
        Operator ->
            true;
        simultaneously when Type == inclusive ->
            true;
        _ ->
            false
    end.

nvl(Val) ->
    nvl(Val, 0).

nvl(undefined, Default) ->
    Default;
nvl(Val, _) ->
    Val.
