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
    case collect_qq_data(QName) of
        {ok, QProcState} ->
            ok = collect_consumer_data(QProcState);
        Error ->
            Error
    end.

% (rabbit-1@PROKOFIEV)8> ra:members(QPid).
% {ok,[{'%2F_RMQ-1280-0','rabbit-1@PROKOFIEV'}, {'%2F_RMQ-1280-0','rabbit-2@PROKOFIEV'}, {'%2F_RMQ-1280-0','rabbit-3@PROKOFIEV'}], {'%2F_RMQ-1280-0','rabbit-1@PROKOFIEV'}}
collect_qq_data(QName) when is_atom(QName) ->
    QPid = whereis(QName),
    Node = node(),
    QPidNode = node(QPid),
    QLeaderResult = case QPidNode of
                        Node ->
                            QProcInfo = recon:info(QName),
                            QProcState = recon:get_state(QName),
                            case QProcState of
                                {leader, _} ->
                                    FName = qq_fname(QName, Node, leader),
                                    file:delete(FName),
                                    file:write_file(FName, io_lib:format("~p~n", [os:system_time(millisecond)])),
                                    file:write_file(FName, io_lib:format("--------~n~p~n", [QProcInfo]), [append]),
                                    file:write_file(FName, io_lib:format("--------~n~p~n", [QProcState]), [append]),
                                    {ok, QProcState};
                                _ ->
                                    {error, "please run from leader node"}
                            end;
                        undefined ->
                            {error, "please run from leader node"};
                        _ ->
                            {error, "please run from leader node"}
                    end,
    case QLeaderResult of
        {ok, QLeaderProcState} ->
            {ok, RaMembers, _RaLeader} = ra:members(QPid),
            [begin
                 QFollowerProcInfo = erpc:call(QNode, recon, info, [QName]),
                 QFollowerProcState = erpc:call(QNode, recon, get_state, [QName]),
                 {follower, _} = QFollowerProcState,
                 FName0 = qq_fname(QName, QNode, follower),
                 file:delete(FName0),
                 file:write_file(FName0, io_lib:format("~p~n", [os:system_time(millisecond)])),
                 file:write_file(FName0, io_lib:format("--------~n~p~n", [QFollowerProcInfo]), [append]),
                 file:write_file(FName0, io_lib:format("--------~n~p~n", [QFollowerProcState]), [append])
             end || {_, QNode} <- RaMembers, QNode =/= Node],
            {ok, QLeaderProcState};
        Error ->
            Error
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
         file:delete(FName),
         file:write_file(FName, io_lib:format("~p~n", [os:system_time(millisecond)])),
         file:write_file(FName, io_lib:format("--------~n~p~n", [PidInfo]), [append]),
         file:write_file(FName, io_lib:format("--------~n~p~n", [recon:get_state(Pid)]), [append])
     end || {CTag, Pid} <- maps:keys(ConsumerMap)],
    ok.

qq_fname(QName, Node, LeaderOrFollower) ->
    Arg = [atom_to_binary(QName), <<"-">>,
           atom_to_binary(LeaderOrFollower),
           <<"-">>,
           atom_to_binary(Node),
           <<".data.txt">>],
    unicode:characters_to_binary(Arg, latin1).

consumer_fname(CTag, Node) ->
    Arg = [CTag, <<"-">>, atom_to_binary(Node), <<".data.txt">>],
    unicode:characters_to_binary(Arg, latin1).
