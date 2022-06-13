
MAKEFLAGS += -s
# If the first argument is "test"...
ifeq (test,$(firstword $(MAKECMDGOALS)))
  # use the rest as arguments for "test"
  RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  # ...and turn them into do-nothing targets
   $(eval $(RUN_ARGS):dummy)
endif
.PHONY: test
build: lucid.p4 lucid.py lucid.cpp
	./libs/p4tapp.sh build lucid.p4
hw:
	./libs/p4tapp.sh hw lucid.p4 ports_up
test:
	./libs/p4tapp.sh test lucid.p4 $(RUN_ARGS)
hw:
	./libs/p4tapp.sh hw lucid.p4 ports_up
dummy: