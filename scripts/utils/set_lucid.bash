# run . ./set_lucid.bash to set the $LUCID 
# environment variable to the root of the lucid repo, 
# by finding the dpt.opam file that must be there.
# this script must be kept in a subdirectory of your 
# active lucid repo to work properly!
cd "$(dirname "${BASH_SOURCE[0]}")"
dpt_opam=$(./findfile.sh dpt.opam)
export LUCID=$(dirname "$dpt_opam")