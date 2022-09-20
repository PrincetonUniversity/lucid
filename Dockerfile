# this dockerfile defines two images: 
# 1. builder -- image with complete ocaml library to build lucid
# 2. production -- lightweight image with just the lucid binaries

# based on: https://medium.com/@bobbypriambodo/lightweight-ocaml-docker-images-with-multi-stage-builds-f7a060c7fce4

# usage: 
# build local image:
# docker build -t lucid .
# rebuild and push published image (for admin only)
# docker build -t lucid .
# docker image tag lucid jsonch/lucid:lucid
# docker push jsonch/lucid:lucid

# --- docker image to build lucid ---
FROM ocaml/opam:alpine AS builder
MAINTAINER John Sonchack
WORKDIR lucid

# install lucid dependencies
ADD dpt.opam .
RUN opam pin add -yn dpt . && \
    opam depext dpt && \
    opam install --deps-only dpt

# add source files
ADD src ./src
ADD Makefile .
ADD dune-project .
ADD dune-workspace .

# build lucid 
RUN sudo chown -R opam:nogroup . && \
    opam config exec make all
# save list of system dependencies
RUN opam depext -ln dpt > depexts
    #| egrep -o "\-\s.*" | sed "s/- //" > depexts
# copy z3 to local dir
RUN cd ../; cp "`pwd`/`find .opam -name 'libz3.so'`" ./lucid


# --- docker image to run lucid ---
FROM alpine AS production_lucid
WORKDIR /app
COPY --from=builder /home/opam/lucid/depexts depexts
RUN cat depexts | xargs apk --update add && rm -rf /var/cache/apk/*
COPY --from=builder /home/opam/lucid/libz3.so /usr/lib/libz3.so

COPY --from=builder /home/opam/lucid/dpt dpt
COPY --from=builder /home/opam/lucid/dptc dptc
COPY --from=builder /home/opam/lucid/bin bin
ADD  tofinoLibs ./tofinoLibs
