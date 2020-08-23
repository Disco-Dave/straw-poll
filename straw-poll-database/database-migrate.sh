#!/bin/bash

set -e

host=${STRAW_POLL_PG_HOST:-localhost}
port=${STRAW_POLL_PG_PORT:-7777}
user=${STRAW_POLL_PG_MIGRATE_USER:-postgres}
password=${STRAW_POLL_PG_MIGRATE_PASSWORD:-postgres}
con="host=$host port=$port dbname=straw_poll user=$user password=$password"

until pg_isready -q --host $host --port $port; do
    sleep 1
done

migrate init "$con"
migrate migrate "$con" ./migrations
