sudo apt install -y opam
opam init -y --auto-setup
eval $(opam env --switch=default)
opam switch create 4.12.0 
eval $(opam env --switch=4.12.0)
opam switch 4.12.0
opam install -y z3.4.13.0
opam install -y --confirm-level=unsafe-yes --deps-only .
