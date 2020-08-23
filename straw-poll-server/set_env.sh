#!/bin/bash

export STRAW_POLL_CONNECTION_STRING="host=localhost port=7777 dbname=straw_poll user=postgres password=postgres"
export STRAW_POLL_HTTP_PORT=9999
$@
