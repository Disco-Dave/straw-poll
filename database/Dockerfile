FROM haskell:8.8.3 AS build

RUN apt-get update \
 && apt-get install --yes libpq-dev postgresql-client-11 \
 && cabal update \
 && cabal install postgresql-simple-migration



FROM debian:buster-slim

RUN apt-get update \
 && apt-get install --yes libpq-dev postgresql-client-11 \
 && rm -rf /var/lib/apt/lists/*

COPY --from=build /root/.cabal/bin/migrate /usr/local/bin/migrate
COPY ./migrate.sh .

CMD ./migrate.sh
