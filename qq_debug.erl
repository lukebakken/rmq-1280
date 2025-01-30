-module(qq_debug).

-export([run/1, get_num_messages/1]).

run([Node, Vhost, Queue]) ->
    Data0 = unicode:characters_to_list([atom_to_list(Vhost), "_"]),
    Data1 = unicode:characters_to_list([Data0, atom_to_list(Queue)]),
    QName = list_to_atom(Data1),
    case erpc:call(Node, ?MODULE, get_num_messages, [QName]) of
        {ok, R} ->
            Prn = fun(X) ->
                    io:format("~ts|~p~n", X)
                  end,
            lists:foreach(Prn, R);
        Error ->
            io:format(standard_error, "[ERROR] ~p~n", [Error])
    end,
    init:stop().

get_num_messages(QName) when is_atom(QName) ->
    QPid = whereis(QName),
    Node = node(),
    QPidNode = node(QPid),
    case QPidNode of
        Node ->
            {ok, RaMembers, _RaLeader} = ra:members(QPid),
            {ok, [begin
                      {ok, MemOverview, _} = ra:member_overview(Mem),
                      [atom_to_binary(element(2, Mem)), maps:get(num_messages, maps:get(machine, MemOverview))]
                  end || Mem <- RaMembers]};
        undefined ->
            {error, "please run from leader node"};
        _ ->
            {error, "please run from leader node"}
    end.
