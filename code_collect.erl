-module(code_collect).

-export([purge/1, do_purge/0]).

purge([Node]) ->
    Result = erpc:call(Node, code_collect, do_purge, []),
    io:format("[INFO] halting, run result: ~p~n", [Result]),
    init:stop().

do_purge() ->
    R1 = do_purge0(),
    R2 = do_purge0(),
    {ok, R1, R2}.

do_purge0() ->
    R1 = code:purge(collect),
    R2 = code:delete(collect),
    R3 = code:load_file(collect),
    {ok, R1, R2, R3}.
