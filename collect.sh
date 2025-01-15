#!/bin/sh

set -eu
# set -x

now()
{
    date '+%Y-%m-%dT%H%M%S%z'
}

readonly rmq_node="${1:-"rabbit@$(hostname)"}"
readonly vhost="${2:-ecarenext}"
readonly queue="${3:-ecn-fep-zone4}"

rmq_cwd="$(rabbitmqctl --node "$rmq_node" eval 'file:get_cwd().' | sed -e 's/[^"]*"//' -e 's/".*$//')"
readonly rmq_cwd

echo "$(now) [INFO] cwd: $rmq_cwd"

cp -f 'collect.erl' "$rmq_cwd"
cd "$rmq_cwd"
rm -f 'collect.beam'

erlc +debug 'collect.erl'

erl -sname "rmq-1280-$$" -noinput -noshell -eval 'code:purge(collect),halt().'

erl -sname "rmq-1280-$$" -noinput -noshell -s collect run "$rmq_node" "$vhost" "$queue"

tgz="collect-data-$(now).tgz"
readonly tgz

echo "$(now) [INFO] creating archive '$tgz' in '$PWD'"

tar -czf "$tgz" ./*.txt
