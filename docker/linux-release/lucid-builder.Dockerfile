# dockerfile for the linux release build
# Start from the x86_64 version of Ubuntu 20.04 for glibc compatibility
FROM --platform=linux/amd64 ubuntu:20.04

# Set non-interactive mode for apt to prevent it from hanging
ENV DEBIAN_FRONTEND=noninteractive

# 1. Install all system-level dependencies from your script
# We're combining the PPA, opam, build-essential, and patchelf
# logic into one layer. I've also added libgmp-dev and libz3-dev,
# as opam will almost certainly require them as system "depexts"
# to install its own Z3 and GMP packages.
RUN apt-get update && apt-get install -y \
    software-properties-common \
    ca-certificates \
    git \
    && add-apt-repository -y ppa:avsm/ppa \
    && apt-get update \
    && apt-get install -y \
    build-essential \
    opam \
    patchelf \
    libgmp-dev \
    libz3-dev \
    && rm -rf /var/lib/apt/lists/*

# 2. Initialize opam and install OCaml 4.12.0
# This mirrors your script's logic: init opam first, then
# create the 4.12.0 switch. We use 'eval $(opam env)' to ensure
# all commands in this layer run inside the opam environment.
RUN opam init -y --auto-setup \
    && eval $(opam env) \
    && opam switch create 4.12.0

# 3. Install Lucid's OCaml dependencies
# We set a work directory...
WORKDIR /build

# ...and copy *only* the dpt.opam file.
# This lets Docker cache this layer. It will only re-run
# this step if you change dpt.opam.
COPY dpt.opam .

# Finally, install the dependencies, including your pinned Z3
RUN eval $(opam env) && opam install -y --confirm-level=unsafe-yes --deps-only .

# 4. Set up the container's environment
# This configures the container so that any command
# you run (like ./build_linux.sh) is automatically
# executed inside the correct opam environment.
# ENV OPAMUTF8MSGS 1
CMD ["/bin/bash", "-c", "eval $(opam env) && /bin/bash"]