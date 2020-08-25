FROM haskell:8.8.3 AS build

RUN apt-get update \
 && apt-get install --yes libpq-dev \
 && mkdir -p /opt/build
WORKDIR /opt/build

COPY stack.yaml stack.yaml.lock package.yaml ./
RUN stack build --system-ghc --dependencies-only

COPY . ./
RUN stack build --system-ghc --copy-bins --local-bin-path ./dist



FROM debian:buster-slim

RUN apt-get update \
 && apt-get install --yes libpq-dev \
 && rm -rf /var/lib/apt/lists/*

COPY --from=build /opt/build/dist/straw-poll-server-exe ./

CMD ./straw-poll-server-exe
