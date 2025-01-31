-module(code_purger).

-export([purge/1, do_purge/1]).

purge([Node, Module]) when is_atom(Node), is_atom(Module) ->
    Result = erpc:call(Node, ?MODULE, do_purge, [Module]),
    io:format("[INFO] halting, run result: ~p~n", [Result]),
    init:stop().

do_purge(Module) ->
    R1 = do_purge0(Module),
    R2 = do_purge0(Module),
    {ok, R1, R2}.

do_purge0(Module) ->
    R1 = code:purge(Module),
    R2 = code:delete(Module),
    R3 = code:load_file(Module),
    {ok, R1, R2, R3}.
