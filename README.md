# Transfer QQ leadership to new node

* Run `rabbitmq-diagnostics remote_shell` on node that is current leader.
* Run the following:

    ```
    {ok, Q} = rabbit_amqqueue:lookup(<<"QUEUE_NAME">>,<<"VHOST_NAME">>),
    ```
* This is example output from my environment. Note that the current leader is the first member of the `nodes` list. Pick another node to become the leader.
    ```
    {ok,{amqqueue,{resource,<<"/">>,queue,<<"qq-0">>},
                true,false,none,
                [{<<"x-queue-type">>,longstr,<<"quorum">>}],
                {'%2F_qq-0','rabbit-3@PROKOFIEV'},
                [],[],[],undefined,undefined,[],[],live,0,[],<<"/">>,
                #{user => <<"guest">>},
                rabbit_quorum_queue,
                #{nodes =>
                        ['rabbit-1@PROKOFIEV','rabbit-3@PROKOFIEV',
                        'rabbit-2@PROKOFIEV']}}}
    ```
* Then, do the transfer:

    ```
    (rabbit-1@PROKOFIEV)26> rabbit_quorum_queue:transfer_leadership(Q, 'rabbit-2@PROKOFIEV').
    {migrated,'rabbit-2@PROKOFIEV'}.
    ```
