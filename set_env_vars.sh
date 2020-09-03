#!/bin/bash

export STRAW_POLL_HTTP_PORT=9999
export STRAW_POLL_PG_USER=postgres
export STRAW_POLL_PG_PASSWORD=postgres
export STRAW_POLL_PG_PORT=7777
export STRAW_POLL_PG_HOST=localhost
export STRAW_POLL_PG_MIGRATE_USER=postgres
export STRAW_POLL_PG_MIGRATE_PASSWORD=postgres
export STRAW_POLL_API_URL="http://localhost:9999"

$@
