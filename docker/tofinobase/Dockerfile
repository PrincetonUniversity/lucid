# build: docker build -t tofinobase .
# run: docker run --privileged -it tofinobase
FROM jsonch/lucid:ocamlbase
MAINTAINER John Sonchack
USER opam:opam

# copy the barefoot sde file, set_sde.bash, and sde config
COPY --chown=opam:opam bf-sde-9.7.2.tgz ./ 
# COPY --chown=opam:opam bf-sde-9.5.1.tgz
COPY --chown=opam:opam set_sde.bash ./
COPY --chown=opam:opam lucid.yaml ./
COPY --chown=opam:opam start_with_hugepages.sh ./

# set up build directory
RUN chmod a+x start_with_hugepages.sh && \
    tar -xzf bf-sde-9.7.2.tgz && \
    cp set_sde.bash bf-sde-9.7.2/ && \
    cp lucid.yaml bf-sde-9.7.2/p4studio/profiles/

# build the sde
RUN cd bf-sde-9.7.2/p4studio && \
    sudo -E ./install-p4studio-dependencies.sh && \
    ./p4studio profile apply ./profiles/lucid.yaml


# startup configuration
SHELL ["/bin/bash", "-c"]
# set environment variables for opam and tofino sde
RUN echo "cd bf-sde-9.7.2; . ./set_sde.bash; cd -; eval $(opam env)" >> ~/.bashrc 
# set hugepages upon entry
ENTRYPOINT ["./start_with_hugepages.sh"]

# note: for start_with_hugepages.sh to succeed, the docker container 
# must either be: 
# 1) run in --priviledged mode OR
# 2) hugepages must be set up on the host VM, 
#    AND /mnt/huge must be shared (-v /mnt/huge:/mnt/huge)
