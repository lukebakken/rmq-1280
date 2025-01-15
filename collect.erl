-module(collect).

-export([run/2]).

run(Vhost, Queue) ->
    QName = list_to_atom(Vhost ++ "_" ++ Queue),
    {ok, QProcState} = collect_qq_data(QName),
    ok = collect_consumer_data(QProcState).

collect_qq_data(QName) when is_atom(QName) ->
    QProcInfo = recon:info(QName),
    QProcState = recon:get_state(QName),
    {LeaderOrFollower, _} = QProcState,
    FName = qq_fname(QName, LeaderOrFollower),
    file:write_file(FName, io_lib:format("~p~n", [QProcInfo])),
    file:write_file(FName, io_lib:format("--------~n~p~n", [QProcState]), [append]),
    {ok, QProcState}.

collect_consumer_data(QProcState) ->
    {_LeaderOrFollower, State} = QProcState,
    StateMap = element(3, State),
    MachineState = maps:get(machine_state, StateMap),
    ConsumerMap = element(10, MachineState),
    [begin
         PidNode = node(Pid),
         PidInfo = erpc:call(PidNode, recon, info, [Pid], 5000),
         FName = consumer_fname(CTag, PidNode),
         file:write_file(FName, io_lib:format("~p~n", [PidInfo])),
         file:write_file(FName, io_lib:format("--------~n~p~n", [recon:get_state(Pid)]), [append])
     end || {CTag, Pid} <- maps:keys(ConsumerMap)],
    ok.

qq_fname(QName, LeaderOrFollower) ->
    Arg = [atom_to_binary(QName), <<"-">>, atom_to_binary(LeaderOrFollower), <<".data.txt">>],
    unicode:characters_to_binary(Arg, latin1).

consumer_fname(CTag, Node) ->
    Arg = [CTag, <<"-">>, atom_to_binary(Node), <<".data.txt">>],
    unicode:characters_to_binary(Arg, latin1).
