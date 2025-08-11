-module(bunnyleek_test).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    SizeOfBigBin = 1024,
    BigBin = binary:copy(<<0>>, SizeOfBigBin),
    SmallBin = binary:part(BigBin, 64, 128),
    Opts = #{
        factor => 5,
        log => fun(RefSize, Bin, Path) ->
            throw({found, RefSize, Bin, Path})
        end
    },
    ?assertException(
        throw,
        {found, RefSize, SmallBin, []} when RefSize == SizeOfBigBin,
        bunnyleek:inspect(SmallBin, Opts)
    ),
    ok = bunnyleek:inspect(<<>>),
    ok = bunnyleek:inspect(make_ref()),
    ?assertException(error, {badarg, factor}, bunnyleek:inspect([], #{factor => a})),
    ?assertException(error, badarg, bunnyleek:inspect_ets(make_ref())).

limit_test() ->
    SizeOfBigBin = 1024,
    BigBin = binary:copy(<<0>>, SizeOfBigBin),
    SmallBin = binary:part(BigBin, 64, 128),
    Opts = #{factor => 0, limit => 2},
    T = [SmallBin, SmallBin, SmallBin],
    ?assertEqual(limit, bunnyleek:inspect(T, Opts)).

ets_test() ->
    T = ets:new(?FUNCTION_NAME, [set]),
    SizeOfBigBin = 1024,
    BigBin = binary:copy(<<0>>, SizeOfBigBin),
    SmallBin = binary:part(BigBin, 64, 128),
    ets:insert(T, {some_ets_key, #{abc => [fun_test_helper(SmallBin)]}}),
    ets:insert(T, {another_key, <<"ABC">>}),
    Opts = #{
        factor => 5,
        log => fun(RefSize, Bin, Path) ->
            bunnyleek:default_log(RefSize, Bin, Path),
            self() ! {found, RefSize, Bin, Path}
        end
    },
    bunnyleek:inspect_ets(T, Opts),
    ?assertEqual(
        [
            {found, SizeOfBigBin, SmallBin, [
                {list_element, 1},
                fun_env,
                {list_element, 1},
                {map_value_for_key, abc},
                {tuple_element, 2},
                {ets_row_for_key, some_ets_key}
            ]}
        ],
        flush()
    ),
    ets:insert(T, {yet_one, SmallBin}),
    ?assertEqual(limit, bunnyleek:inspect_ets(T, #{limit => 1, factor => 2})).

flush() ->
    flush([]).

flush(Acc) ->
    receive
        Msg ->
            flush([Msg | Acc])
    after 0 ->
        Acc
    end.

fun_test_helper(Bin) ->
    fun() ->
        Bin
    end.
