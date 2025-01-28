#!/bin/sh

set -eu
set -x

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

rmq_cwd="$(rabbitmqctl --node "$rmq_node" eval 'file:get_cwd().' | sed -e 's/[^"]*"\([^"]\+\)".*/\1/')"
readonly rmq_cwd

echo "$(now) [INFO] cwd: $rmq_cwd"

cp -f 'collect.erl' "$rmq_cwd"
cd "$rmq_cwd"
rm -f 'collect.beam'

erlc +debug 'collect.erl'

erl -sname "rmq-1280-$$" -noinput -noshell -eval 'code:purge(collect),halt().'

erl -sname "rmq-1280-$$" -noinput -noshell -s collect run "$rmq_node" "$rmq_vhost" "$rmq_queue"

tgz="collect-data-$rmq_queue-$(now)-$(now_unix).tgz"
readonly tgz

echo "$(now) [INFO] creating archive '$tgz' in '$PWD'"

tar -czf "$tgz" ./*.txt

if [ -z "$COLLECT_DEBUG" ]
then
    rm -f ./*.data.txt
    rm -f ./collect.erl
    rm -f ./collect.beam
fi
