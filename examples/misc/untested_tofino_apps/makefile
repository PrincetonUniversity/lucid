FIND_DPTC := ./findfile.sh dptc
DPTC := $(shell $(FIND_DPTC) || echo "echo 'dptc not found'; exit 1" | sh)


ifndef SRC
SRC := $(firstword $(wildcard *.dpt))
endif

# Define DPT_BUILD_DIR based on the absolute path of SRC and the base name of the file
DPT_BUILD_DIR := $(dir $(abspath $(SRC)))build_$(basename $(notdir $(SRC)))

.PHONY: all clean dptc force current_makefile

OUTPUT_FILE := $(DPT_BUILD_DIR)/lucid.p4

all: $(OUTPUT_FILE)

$(OUTPUT_FILE): $(SRC)
	$(DPTC) $< -o $(DPT_BUILD_DIR)

clean:
	rm -rf $(DPT_BUILD_DIR)

dptc:
	cd $(dir $(DPTC)) && make

force: current_makefile
	touch $(SRC)
	$(MAKE) all -f $(call current_makefile) SRC=$(SRC)

current_makefile:
	@echo $(lastword $(MAKEFILE_LIST))
