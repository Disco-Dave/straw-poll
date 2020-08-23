FROM haskell:8.8.3

RUN apt-get update
RUN cabal update
RUN apt-get install --yes libpq-dev postgresql-client-11
RUN cabal install postgresql-simple-migration

COPY ./database-migrate.sh .
COPY ./migrations migrations

CMD ./database-migrate.sh
