.PHONY: interp compile assemble test

PROG := reflector
DPT := ../dpt
DPTC := ../dptc

# run interpreter
interp: 
	cd ../; make
	$(DPT) $(PROG).dpt

# compile Lucid to P4
compile: 
	cd ../; make
	$(DPTC) $(PROG).dpt ip_harness.p4 ip_harness.json build

# compile P4 to tofino
assemble:
	cd build; make build	 

# test P4 on the asic model
test:
	cd build; make test ../$(PROG).json