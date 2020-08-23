#!/bin/bash

container_name=straw_poll_dev_database

if [ "$(docker ps -aq -f name=$container_name)" ]; then
    docker stop $container_name
fi
