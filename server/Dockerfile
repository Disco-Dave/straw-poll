FROM straw-poll-haskell-deps:latest AS build

COPY . ./
RUN stack build --system-ghc --copy-bins --local-bin-path ./dist



FROM debian:buster-slim

RUN apt-get update \
 && apt-get install --yes libpq-dev \
 && rm -rf /var/lib/apt/lists/*

COPY --from=build /opt/build/dist/straw-poll-server-exe ./

CMD ./straw-poll-server-exe
