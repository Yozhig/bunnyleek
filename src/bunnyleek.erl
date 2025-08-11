%%% Helps to find sub binaries that reference much bigger binary in ets or any given term.
%%% Sample usage (shows at most 10 sub binaries that reference binaries 5 times bigger):
%%%   bunnyleek:inspect_ets(Table, #{factor => 5, limit => 10}).
-module(bunnyleek).

-export([
    inspect/1,
    inspect/2,
    inspect_ets/1,
    inspect_ets/2
]).

-type user_opts() :: #{
    factor => number(), % default is 10
    limit => pos_integer(), % default is 100
    log => log_fun()
}.
-export_type([user_opts/0]).

-define(DEFAULT_FACTOR, 10).
-define(DEFAULT_LIMIT, 100).

-type log_fun() :: fun((RefSize :: non_neg_integer(), BinPart :: binary(), Path :: path()) -> Ignored ::any()).
-export_type([log_fun/0]).

-type path() :: [step()].
-export_type([path/0]).

-type step() :: {ets_row_for_key, term()}
              | {tuple_element, pos_integer()}
              | {list_element, pos_integer()}
              | {map_key, term()} % key contains binary itself
              | {map_value_for_key, term()} % value (can be found by given key) contains binary
              | fun_env. % function scope (bound variable)
-export_type([step/0]).

-type state() :: #{
    factor := number(),
    limit := pos_integer(),
    counter := atomics:atomics_ref(), % smells, but ok for this script
    log := log_fun()
}.

-ifdef(TEST).

-export([default_log/3]).

-endif. % TEST

%%% API

-spec inspect(term()) -> ok | limit.
inspect(T) ->
    inspect(T, #{}).

-spec inspect(term(), user_opts()) -> ok | limit.
inspect(T, UserOpts) ->
    State = state(UserOpts),
    try
        inspect(T, State, [])
    catch throw:limit ->
        limit
    end.

-spec inspect_ets(ets:tab()) -> ok | empty | limit.
inspect_ets(Table) ->
    inspect_ets(Table, #{}).

-spec inspect_ets(ets:tab(), user_opts()) -> ok | empty | limit.
inspect_ets(Table, UserOpts) ->
    State = state(UserOpts),
    KeyPos = ets:info(Table, keypos),
    try
        ets:foldl(
            fun(Tuple, _) ->
                inspect(Tuple, State, [{ets_row_for_key, element(KeyPos, Tuple)}])
            end,
            empty,
            Table
        )
    catch throw:limit ->
        limit
    end.

%%% Internals

-spec state(user_opts()) -> state().
state(Opts) ->
    #{
        factor => opt(factor, Opts, fun is_number/1, ?DEFAULT_FACTOR),
        limit => opt(limit, Opts, fun(L) -> is_integer(L) andalso L > 0 end, ?DEFAULT_LIMIT),
        counter => atomics:new(1, []),
        log => opt(log, Opts, fun(F) -> is_function(F, 3) end, fun default_log/3)
    }.

-spec opt(Key :: term(), Opts :: user_opts(), Verify :: fun((Valut :: term()) -> boolean()), Default) -> Result when
    Default :: T,
    Result :: T,
    T :: dynamic().
opt(Key, Opts, Verify, _Default) when is_map_key(Key, Opts) ->
    Value = maps:get(Key, Opts),
    case Verify(Value) of
        true ->
            Value;
        _ ->
            error({badarg, Key})
    end;
opt(_Key, _Opts, _Verify, Default) ->
    Default.

-spec default_log(non_neg_integer(), binary(), path()) -> any().
default_log(RefSize, Bin, Path) ->
    io:format(group_leader(), "found ref size ~w: ~p;~npath: ~p~n", [RefSize, Bin, Path]).

-spec inspect(term(), state(), path()) -> ok | no_return().
inspect(Tuple, State, Path) when is_tuple(Tuple) ->
    tuple_foreach(
        fun(Element, N) ->
            inspect(Element, State, [{tuple_element, N} | Path])
        end,
        Tuple
    );
inspect(List, State, Path) when is_list(List) ->
    maybe_improper_list_foreach(
        fun(Element, N) ->
            inspect(Element, State, [{list_element, N} | Path])
        end,
        List
    );
inspect(M, State, Path) when is_map(M) ->
    maps:foreach(
        fun(K, V) ->
            % FIXME: key can be huge
            inspect(K, State, [{map_key, K} | Path]),
            inspect(V, State, [{map_value_for_key, K} | Path])
        end,
        M
    );
inspect(Fun, State, Path) when is_function(Fun) ->
    {env, Env} = fun_info(Fun, env),
    inspect(Env, State, [fun_env | Path]);
inspect(Bin, #{factor := Factor, log := Log, counter := Counter, limit := Limit}, Path) when is_binary(Bin) ->
    RefSize = binary:referenced_byte_size(Bin),
    case RefSize > byte_size(Bin) * Factor of
        true ->
            _ = Log(RefSize, Bin, Path),
            case atomics:add_get(Counter, 1, 1) of
                NewCount when NewCount >= Limit ->
                    throw(limit);
                _ ->
                    ok
            end;
        false ->
            ok
    end;
inspect(_OtherTerm, _State, _Path) ->
    ok.

tuple_foreach(Fun, Tuple) ->
    tuple_foreach(Fun, Tuple, 1).

tuple_foreach(_Fun, Tuple, N) when N > tuple_size(Tuple) ->
    ok;
tuple_foreach(Fun, Tuple, N) ->
    _ = Fun(element(N, Tuple), N),
    tuple_foreach(Fun, Tuple, N + 1).

% Makes eqwalizer happy (it doesn't like 'env')
fun_info(Fun, Item) ->
    erlang:fun_info(Fun, Item).

maybe_improper_list_foreach(Fun, List) when is_list(List) ->
    maybe_improper_list_foreach(Fun, List, 1).

maybe_improper_list_foreach(Fun, [E | Rest], N) ->
    _ = Fun(E, N),
    maybe_improper_list_foreach(Fun, Rest, N + 1);
maybe_improper_list_foreach(_Fun, [], _N) ->
    ok;
maybe_improper_list_foreach(Fun, Tail, N) ->
    _ = Fun(Tail, N),
    ok.
