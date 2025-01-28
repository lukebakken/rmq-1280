-module(collect).

-export([run/1, do_run/1]).

run([Node, Vhost, Queue]) ->
    Data0 = unicode:characters_to_list([atom_to_list(Vhost), "_"]),
    Data1 = unicode:characters_to_list([Data0, atom_to_list(Queue)]),
    QName = list_to_atom(Data1),
    io:format("[INFO] VHost ~p Queue ~p QName ~p~n", [Vhost, Queue, QName]),
    Result = erpc:call(Node, collect, do_run, [QName]),
    io:format("[INFO] halting, result: ~p~n", [Result]),
    init:stop().

do_run(QName) when is_atom(QName) ->
    {ok, QProcState} = collect_qq_data(QName),
    ok = collect_consumer_data(QProcState).

collect_qq_data(QName) when is_atom(QName) ->
    QPid = whereis(QName),
    Node = node(),
    QPidNode = node(QPid),
    case QPidNode of
        Node ->
            QProcInfo = recon:info(QName),
            QProcState = recon:get_state(QName),
            {leader, _} = QProcState,
            FName = qq_fname(QName, leader),
            file:write_file(FName, io_lib:format("~p~n", [os:system_time(millisecond)])),
            file:write_file(FName, io_lib:format("--------~n~p~n", [QProcInfo]), [append]),
            file:write_file(FName, io_lib:format("--------~n~p~n", [QProcState]), [append]),
            {ok, QProcState};
        undefined ->
            {error, "please run from leader node"};
        _ ->
            {error, "please run from leader node"}
    end.

collect_consumer_data(QProcState) ->
    {_LeaderOrFollower, State} = QProcState,
    StateMap = element(3, State),
    MachineState = maps:get(machine_state, StateMap),
    ConsumerMap = element(10, MachineState),
    [begin
         PidNode = node(Pid),
         PidInfo = erpc:call(PidNode, recon, info, [Pid], 5000),
         FName = consumer_fname(CTag, PidNode),
         file:write_file(FName, io_lib:format("~p~n", [os:system_time(millisecond)])),
         file:write_file(FName, io_lib:format("--------~n~p~n", [PidInfo]), [append]),
         file:write_file(FName, io_lib:format("--------~n~p~n", [recon:get_state(Pid)]), [append])
     end || {CTag, Pid} <- maps:keys(ConsumerMap)],
    ok.

qq_fname(QName, LeaderOrFollower) ->
    Arg = [atom_to_binary(QName), <<"-">>, atom_to_binary(LeaderOrFollower), <<".data.txt">>],
    unicode:characters_to_binary(Arg, latin1).

consumer_fname(CTag, Node) ->
    Arg = [CTag, <<"-">>, atom_to_binary(Node), <<".data.txt">>],
    unicode:characters_to_binary(Arg, latin1).
