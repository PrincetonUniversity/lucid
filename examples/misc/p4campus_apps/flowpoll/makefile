FIND_DPTC := ./findfile.sh dptc
FIND_DPT := ./findfile.sh dpt
DPTC := $(shell $(FIND_DPTC) || echo "echo 'dptc not found'; exit 1" | sh)
DPT := $(shell $(FIND_DPT) || echo "echo 'dpt not found'; exit 1" | sh)


# input source file. 
ifndef SRC
SRC := $(firstword $(wildcard *.dpt))
endif

# Define DPT_BUILD_DIR based on the absolute path of SRC and the base name of the file
DPT_BUILD_DIR := $(dir $(abspath $(SRC)))build_$(basename $(notdir $(SRC)))

OUTPUT_FILE := $(DPT_BUILD_DIR)/lucid.p4

.PHONY: compile clean dptc force current_makefile interp

interp:
	$(DPT) $(SRC)

compile: $(SRC)
	$(DPTC) $(SRC) --new-tofino -o $(DPT_BUILD_DIR)

clean:
	rm -rf $(DPT_BUILD_DIR)

dptc:
	cd $(dir $(DPTC)) && make
