FROM straw-poll-haskell-deps:latest AS build

RUN stack install --system-ghc --resolver lts-16.18 --copy-bins --local-bin-path /root/dist postgresql-simple-migration


FROM debian:buster-slim

RUN apt-get update \
 && apt-get install --yes libpq-dev postgresql-client-11 \
 && rm -rf /var/lib/apt/lists/*

COPY --from=build /root/dist/migrate /usr/local/bin/migrate
COPY ./migrate.sh .

CMD ./migrate.sh
