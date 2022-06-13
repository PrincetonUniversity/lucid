.PHONY: interp compile assemble test


SOURCE := $(shell basename "*.dpt" .dpt)

lc = "../../dptc" 

# run interpreter
interp: 
	$(MAKE) -C ..
	../dpt $(SOURCE).dpt --spec $(SOURCE).json

# compile Lucid to P4
compile: 
# 	$(MAKE) -C ..
	$(lc) $(SOURCE).dpt ip_harness.p4 linker_config.json build

compile_monolithic: 
	$(MAKE) -C ..
	../dptc starflow_monolithic.dpt.old ip_harness.p4 linker_config.json build


# compile P4 to tofino
assemble:
	cd build; make build	 

# test P4 on the asic model
test:
	cd build; make test ../$(SOURCE)_test.json