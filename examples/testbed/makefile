.PHONY: interp compile assemble test

# compile Lucid to P4
compile: 
	../../dptc wire.dpt ip_harness.p4 linker_config.json wire_build

# compile P4 to tofino
assemble:
	cd wire_build; make build