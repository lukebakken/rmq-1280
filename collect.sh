#!/bin/sh

set -eu
# set -x

if [ ! -s "$HOME/.erlang.cookie" ]
then
    export HOME=/var/lib/rabbitmq
fi

now()
{
    date '+%Y-%m-%dT%H%M%S%z'
}

now_unix()
{
    date '+%s'
}

readonly rmq_node="${1:-"rabbit@$(hostname -s)"}"
readonly rmq_vhost="${2:-ecarenext}"
readonly rmq_queue="${3:-ecn-fep-zone4}"

if [ "$rmq_vhost" = '/' ]
then
    readonly rmq_enc_vhost='%2F'
else
    readonly rmq_enc_vhost="$rmq_vhost"
fi

rmq_cwd="$(rabbitmqctl --node "$rmq_node" eval 'file:get_cwd().' | sed -e 's/[^"]*"\([^"]\+\)".*/\1/')"
readonly rmq_cwd

echo "$(now) [INFO] cwd: $rmq_cwd"

cp -f code_purger.erl collect.erl "$rmq_cwd"
cd "$rmq_cwd"
rm -f code_purger.beam collect.beam

erlc +debug 'code_purger.erl'
erlc +debug 'collect.erl'
erl -sname "rmq-1280-$$" -noinput -noshell -s code_purger purge "$rmq_node" 'collect'
erl -sname "rmq-1280-$$" -noinput -noshell -s collect run "$rmq_node" "$rmq_enc_vhost" "$rmq_queue"

tgz="collect-data-$rmq_queue-$(now)-$(now_unix).tgz"
readonly tgz

rabbitmq-queues --node "$rmq_node" --formatter json --vhost "$rmq_vhost" quorum_status "$rmq_queue" > "quorum_status-$rmq_queue-$(now).json.txt" 2>&1

echo "$(now) [INFO] creating archive '$tgz' in '$PWD'"

tar -czf "$tgz" ./*.txt

set +u
if [ -z "$COLLECT_DEBUG" ]
then
    rm -f ./*.data.txt
    rm -f ./collect.erl
    rm -f ./collect.beam
    rm -f ./code_purger.erl
    rm -f ./code_purger.beam
fi
