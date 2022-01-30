dptf := ../../dptf

.PHONY: test

# Compile lucid code to P4 library
compile: main.dpt
	$(dptf) main.dpt

# Compile p4 program that uses lucid library.
build: main.p4 main.dpt.p4 main.py main.cpp
	./libs/p4tapp.sh build main.p4

# Run test case in tofino asic simulator.
test:
	./libs/p4tapp.sh test main.p4 test.json # $(RUN_ARGS)

# TODO -- run switchd and controller on physical tofino. 
# run