.PHONY: interp compile assemble test

# run interpreter
interp: 
	../../dpt histogram.dpt --spec histogram.json

# compile Lucid to P4
compile: 
	../../dptc histogram.dpt ip_harness.p4 ip_harness_triggers.json hist_build

# compile P4 to tofino
assemble:
	cd hist_build; make build	 

# test P4 on the asic model
test:
	cd hist_build; make test ../histogram_test.json