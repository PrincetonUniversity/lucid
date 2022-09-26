# this dockerfile build an image with 
# all dependencies for lucid + the tofino SDE.

FROM ocaml/opam:ubuntu-18.04-ocaml-4.11 as ocaml_builder
MAINTAINER John Sonchack
USER opam:opam
WORKDIR lucid

# install lucid dependencies
ADD dpt.opam .
RUN opam pin add -yn dpt . && \
    opam depext dpt && \
    opam install --deps-only dpt

FROM ocaml_builder

ARG sde_base=bf-sde-9.7.2
# copy barefoot files
COPY --chown=opam:opam docker/tofino_dev/$sde_base.tgz ./ 
COPY --chown=opam:opam docker/tofino_dev/set_sde.bash ./
COPY --chown=opam:opam docker/tofino_dev/lucid.yaml ./
COPY --chown=opam:opam docker/tofino_dev/start_with_hugepages.sh ./


# set up build directory
RUN chmod a+x start_with_hugepages.sh && \
    tar -xzf $sde_base.tgz && \
    cp set_sde.bash $sde_base/ && \
    cp lucid.yaml $sde_base/p4studio/profiles/

# install sde dependencies 
RUN cd $sde_base/p4studio && \
    sudo -E ./install-p4studio-dependencies.sh

# build the sde
RUN cd $sde_base/p4studio && \
    ./p4studio profile apply ./profiles/lucid.yaml

# install a few more dependencies
RUN pip3 install dpkt

RUN sudo apt-get update -y && sudo apt-get install -y \
    software-properties-common \
    m4 \
    curl \
    make \
    build-essential \
    libgmp-dev \
    pkg-config \
    tcpreplay \
    expect && \

# use bash as the shell, so we can use bashrc
SHELL ["/bin/bash", "-c"]
# set environment variables for opam and tofino sde
RUN echo "cd $sde_base; . ./set_sde.bash; cd -; eval $(opam env)" >> ~/.bashrc

ENTRYPOINT ["./start_with_hugepages.sh"]
