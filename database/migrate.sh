#!/bin/bash

set -e

host=${STRAW_POLL_PG_HOST:-localhost}
port=${STRAW_POLL_PG_PORT:-7777}
user=${STRAW_POLL_PG_MIGRATE_USER:-postgres}
password=${STRAW_POLL_PG_MIGRATE_PASSWORD:-postgres}
con="host=$host port=$port dbname=straw_poll user=$user password=$password"

sleep_time=0
timeout=60

until (( sleep_time >= timeout )) || pg_isready -q --host $host --port $port; do
    >&2 echo "Postgres is unavailable - sleeping"
    sleep_time=$((sleep_time + 1))
    sleep 1
done

if (( sleep_time >= timeout )); then
    >&2 echo "Timed out - unable to connect to Postgres"
    exit 1
fi

>&2 echo "Postgres is available - executing migrations"

migrate init "$con"
migrate migrate "$con" ./migrations
