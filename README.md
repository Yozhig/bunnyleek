bunnyleek
=========

Simple script which helps to find sub binaries that reference much bigger binary in ets or almost any term.
It can't answer a question how much memory you can (or can't) save,
should you copy your binaries or not (because it depends on your load).
It just shows places.

If you are looking for places where binaries have been allocated (since OTP 27):
```shell
$ erl +Muatags code
> instrument:allocations(#{flags => [per_mfa]}).
```
which is not very helpful because it doesn't show you where sub binaries were created.

Also take a look at [recon:bin_leak/1](https://ferd.github.io/recon/recon.html#bin_leak/1).

Usage
-----
```erlang
% show at most 10 sub binaries that reference binaries 5 times bigger
bunnyleek:inspect_ets(Table, #{factor => 5, limit => 10}).
% NOTE: it will iterate over the whole Table when there are no small enough sub biniries
```
or
```erlang
bunnyleek:inspect(Term).
```
or even
```erlang
Inspect = fun(YourTable) ->
    IO = group_leader(),
    T = ets:new(test, [set]),
    Log = fun(RefSize, Bin, _Path) ->
        Size = byte_size(Bin),
        ets:update_counter(T, RefSize, Size, {RefSize, 0})
    end,
    Opts = #{factor => 2, limit => 1 bsl 32, log => Log},
    bunnyleek:inspect_ets(YourTable, Opts),
    io:format(IO, "~nvery inaccurate number of big binaries: ~w~n", [ets:info(T, size)]),
    Sum = ets:foldl(
        fun({Total, Subs}, Acc) ->
            Acc + Total - Subs
        end,
        0,
        T
    ),
    case Sum > 0 of
        true -> io:format(IO, "~nyou can save at least ~w bytes!~n", [Sum]);
        false -> io:format(IO, "~nseems like you'll waste additional ~w bytes by copying (but not sure)~n", [abs(Sum)])
    end,
    ets:delete(T)
end.
Inspect(my_table).
```
and result:
```
very inaccurate number of big binaries: 6

you can save at least 265 bytes!
```
Note that we count all big binaries with the same size as one which is not correct.
If you know other options, open an issue or pr please.

Even if you get negative result right now this does not mean you will not save memory in the long run.
If you have regular updates, some new sub binaries can start point to new big binaries
while other remain holding refs to old ones.

TLDR: add `binary:copy/1` and test with your data and load.
