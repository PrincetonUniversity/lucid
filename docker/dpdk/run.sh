#!/bin/sh
# Run a command in the lucid-dpdk image with DIR mounted as /work (the
# current directory inside): docker/dpdk/run.sh DIR CMD [ARGS...]
# e.g. docker/dpdk/run.sh BUILD_DIR make, or ... sh run_test.sh
# NET_ADMIN + IPv6 off: for veth tests (af_packet) inside the container.
set -eu
dir=$1; shift
exec docker run --rm --platform linux/amd64 --cap-add NET_ADMIN \
  --sysctl net.ipv6.conf.all.disable_ipv6=1 --sysctl net.ipv6.conf.default.disable_ipv6=1 \
  -v "$(cd "$dir" && pwd):/work" -w /work lucid-dpdk "$@"
