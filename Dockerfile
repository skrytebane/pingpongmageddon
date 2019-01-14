FROM debian:9.6 as builder

RUN apt-get update && apt-get install -y ghc libghc-random-dev && apt-get clean

RUN mkdir /pingpongmageddon
COPY generate_brackets.hs /pingpongmageddon
RUN ghc -Wall -optl-static -optl-pthread /pingpongmageddon/generate_brackets.hs
RUN strip /pingpongmageddon/generate_brackets

FROM alpine:3.8

WORKDIR /root
COPY --from=builder /pingpongmageddon/generate_brackets .

CMD ["./generate_brackets"]
