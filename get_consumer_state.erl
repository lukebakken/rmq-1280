-module(get_consumer_state).

-export([run/2]).

run(Vhost, Queue) ->
    QueuePidName = list_to_atom(Vhost ++ "_" ++ Queue),
    {LeaderOrFollower, State} = recon:get_state(QueuePidName),
    case LeaderOrFollower of
      Value when Value =:= leader orelse Value =:= follower ->
        io:format("[INFO] running on ~p node~n",[Value]);
      Huh ->
        io:format("[ERROR] unknown type ~p~n",[Huh])
    end,
    StateMap = element(3, State),
    MachineState = maps:get(machine_state, StateMap),
    ConsumerMap = element(10, MachineState),
    GenFileName = fun(ArgCtag) ->
                          unicode:characters_to_binary([ArgCtag,<<"-">>,atom_to_binary(LeaderOrFollower),<<".state.txt">>], latin1)
                  end,
    [file:write_file(GenFileName(Ctag), io_lib:format("~p~n", [recon:get_state(Pid)])) || {Ctag, Pid} <- maps:keys(ConsumerMap)].