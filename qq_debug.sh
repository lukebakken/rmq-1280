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

rmq_cwd="$(rabbitmqctl --node "$rmq_node" eval 'file:get_cwd().' | sed -e 's/[^"]*"\([^"]\+\)".*/\1/')"
readonly rmq_cwd

cp -f qq_debug.erl "$rmq_cwd"
cd "$rmq_cwd"
rm -f qq_debug.beam

erlc +debug 'qq_debug.erl'
erl -sname "rmq-1280-$$" -noinput -noshell -s qq_debug run "$rmq_node" "$rmq_vhost" "$rmq_queue"

set +u
if [ -z "$QQ_DEBUG_DEBUG" ]
then
    rm -f ./qq_debug.erl
    rm -f ./qq_debug.beam
fi
