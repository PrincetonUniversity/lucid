objects = \
reflector dropper \
control_reflector control_dropper \
control_duplicator recursive_loop \
state_loop direct_index hash_index \
array_args simple_stateful_firewall \
two_stage_stateful_firewall \
simple_cuckoo_firewall

# compile lucid to P4
$(objects):  %: %.dpt
	{ \
	cd ../../ ;\
	make ;\
	cd - ;\
	../../dptc $@.dpt ip_harness.p4 build ;\
	}

.PHONY: build

# compile P4 to tofino
build:
	cd build; make build	 

# test with different traces
test_onepkt:
	cd build; make test ../traces/onepkt.json 128 128
test_twopkts: 
	cd build; make test ../traces/twopkts.json 128 128
test_authpkts: 
	cd build; make test ../traces/authpkts.json 128 128
test_threecolliding: 
	cd build; make test ../traces/threecolliding.json 128 128