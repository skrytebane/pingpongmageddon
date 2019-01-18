FROM debian:9.6 as builder

RUN apt-get update && \
    apt-get install -y ghc libghc-random-dev libghc-aeson-dev libghc-optparse-applicative-dev cabal-install && \
    apt-get clean

RUN mkdir /pingpongmageddon
WORKDIR /pingpongmageddon
COPY . /pingpongmageddon
RUN cabal configure \
        --disable-executable-dynamic \
        --disable-shared \
        --ghc-option=-optl=-static \
        --ghc-option=-optl=-pthread
RUN cabal build

FROM alpine:3.8

COPY --from=builder \
        /pingpongmageddon/dist/build/pingpongmageddon-exe/pingpongmageddon-exe \
        /bin/pingpongmageddon

CMD ["/bin/pingpongmageddon"]
