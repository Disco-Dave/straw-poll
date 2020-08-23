#!/bin/bash

container_name=straw_poll_dev_database

if [ "$(docker ps -aq -f name=$container_name)" ]; then
    docker rm --force $container_name
fi

if [ -d .postgres-data ]; then
    rm -rf .postgres-data
fi
