#!/bin/bash

set -e

docker build -t straw-poll-haskell-deps -f server/Dockerfile.deps ./server
docker-compose build
