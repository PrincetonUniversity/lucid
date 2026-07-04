#!/usr/bin/env bash
# Helper for the Lucid dev-environment container.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOCKERFILE="$SCRIPT_DIR/Dockerfile"
PROG="$(basename "$0")"

IMAGE="lucid-dev"                                
REMOTE="ghcr.io/princetonuniversity/$IMAGE"         
TAG="${TAG:-latest}"                         # tag to pull/publish
# Build platform, optional (defaults to host-native), e.g. 
# PLATFORM=linux/amd64 ./dockercmd.sh build
PLATFORM="${PLATFORM:-}"
HOME_DIR="/home/ubuntu"
CONTAINER="$IMAGE"  # name of the persistent background container

usage() {
    cat <<EOF
Helper for the Lucid dev-environment container.

Usage:
  $PROG build              Build the image locally.
  $PROG enter [PATH]       Start an interactive shell in a throwaway
                           container. If PATH is given, that file or
                           directory is bind-mounted into the home dir.
  $PROG up [PATH]          Start the named '$CONTAINER' container in the
                           background (for IDE attach). PATH is mounted as
                           with 'enter'. Idempotent.
  $PROG exec [CMD...]      Run a shell (or CMD) in the background container.
  $PROG down               Stop and remove the background container.
  $PROG pull               Pull the prebuilt image from the registry.
  $PROG publish            Build multi-arch and push to the registry.

Env:
  PLATFORM    build arch override, e.g. linux/amd64 (default: host-native)
  PLATFORMS   publish target arches (default: linux/amd64,linux/arm64)
  TAG         tag for pull/publish (default: latest)
EOF
    exit "${1:-0}"
}

cmd_build() {
    local build_args=(-f "$DOCKERFILE" -t "$IMAGE")
    [[ -n "$PLATFORM" ]] && build_args+=(--platform "$PLATFORM")
    docker build "${build_args[@]}" "$SCRIPT_DIR"
}

# Resolve a path to an absolute path (portable; macOS lacks GNU realpath).
# For directories, cd into them so paths like "." or "../foo" resolve to
# their real name rather than "." / "foo".
abspath() {
    local p="$1"
    if [[ -d "$p" ]]; then
        (cd "$p" && pwd)
    else
        echo "$(cd "$(dirname "$p")" && pwd)/$(basename "$p")"
    fi
}

require_path() { [[ -e "$1" ]] || { echo "error: no such path: $1" >&2; exit 1; }; }

cmd_enter() {
    # --cap-add=NET_ADMIN lets the interpreter create/configure veth interfaces.
    local run_args=(--rm -it --cap-add=NET_ADMIN)

    local path="${1:-}"
    if [[ -n "$path" ]]; then
        require_path "$path"
        local abs; abs="$(abspath "$path")"
        run_args+=(-v "$abs:$HOME_DIR/$(basename "$abs")")
    fi

    docker run "${run_args[@]}" "$IMAGE"
}

# True if the named container exists (any state).
container_exists() { docker ps -a --format '{{.Names}}' | grep -qx "$CONTAINER"; }
# True if the named container is currently running.
container_running() { docker ps --format '{{.Names}}' | grep -qx "$CONTAINER"; }

cmd_up() {
    if container_running; then
        echo "container '$CONTAINER' is already running. Use '$PROG exec' for a shell." >&2
        return 0
    fi
    # Drop a stale stopped container so the new mount/workdir take effect.
    container_exists && docker rm -f "$CONTAINER" >/dev/null

    local run_args=(-d --name "$CONTAINER" --cap-add=NET_ADMIN)
    local workdir="$HOME_DIR"
    local path="${1:-}"
    if [[ -n "$path" ]]; then
        require_path "$path"
        local abs; abs="$(abspath "$path")"
        workdir="$HOME_DIR/$(basename "$abs")"
        run_args+=(-v "$abs:$workdir")
    fi
    run_args+=(-w "$workdir")

    # sleep infinity keeps the container alive for exec/IDE attach.
    docker run "${run_args[@]}" "$IMAGE" sleep infinity >/dev/null
    echo "container '$CONTAINER' is up (workdir: $workdir)."
    echo "Attach your IDE (VSCode: Dev Containers > Attach to Running Container)"
    echo "or run '$PROG exec' for a shell."
}

cmd_exec() {
    container_running || { echo "error: container '$CONTAINER' is not running; start it with '$PROG up [PATH]'." >&2; exit 1; }
    # bash (interactive) sources ~/.bashrc, which loads the opam env.
    if [[ $# -gt 0 ]]; then
        docker exec -it "$CONTAINER" "$@"
    else
        docker exec -it "$CONTAINER" bash
    fi
}

cmd_down() {
    if container_exists; then
        docker rm -f "$CONTAINER" >/dev/null
        echo "container '$CONTAINER' stopped and removed."
    else
        echo "container '$CONTAINER' is not running."
    fi
}

cmd_pull() {
    # Fetch the prebuilt image (Docker picks your arch) and tag it for local use,
    # so `enter` behaves the same whether you built or pulled.
    docker pull "$REMOTE:$TAG"
    docker tag "$REMOTE:$TAG" "$IMAGE"
}

cmd_publish() {
    # Multi-arch build + push. Requires `docker login ghcr.io` first. Uses a
    # buildx builder with the docker-container driver (created here if missing).
    local platforms="${PLATFORMS:-linux/amd64,linux/arm64}"
    docker buildx inspect lucid-builder >/dev/null 2>&1 \
        || docker buildx create --name lucid-builder --driver docker-container >/dev/null
    docker buildx build --builder lucid-builder \
        --platform "$platforms" \
        -f "$DOCKERFILE" -t "$REMOTE:$TAG" --push "$SCRIPT_DIR"
}

main() {
    local cmd="${1:-}"
    [[ $# -gt 0 ]] && shift || true
    case "$cmd" in
        build)   cmd_build "$@" ;;
        enter)   cmd_enter "$@" ;;
        up)      cmd_up "$@" ;;
        exec)    cmd_exec "$@" ;;
        down)    cmd_down "$@" ;;
        pull)    cmd_pull "$@" ;;
        publish) cmd_publish "$@" ;;
        ""|-h|--help|help) usage 0 ;;
        *) echo "error: unknown command: $cmd" >&2; usage 1 ;;
    esac
}

main "$@"