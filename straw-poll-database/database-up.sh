#!/bin/bash

set -e

container_name=straw_poll_dev_database
password=postgres
database=straw_poll
port=7777

if [ ! "$(docker ps -aq -f name=$container_name)" ]; then
    docker run -d \
        --name $container_name \
        -e POSTGRES_PASSWORD=$password \
        -e POSTGRES_DB=$database \
        -p $port:5432 \
        -v "$(pwd)"/.postgres-data:/var/lib/postgresql/data \
        postgres 
elif [ "$(docker ps -aq -f status=exited -f name=$container_name)" ]; then
    docker start $container_name
fi
