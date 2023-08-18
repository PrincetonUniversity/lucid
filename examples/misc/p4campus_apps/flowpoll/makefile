FIND_DPTC := ./findfile.sh dptc
FIND_DPT := ./findfile.sh dpt
DPTC := $(shell $(FIND_DPTC) || echo "echo 'dptc not found'; exit 1" | sh)
DPT := $(shell $(FIND_DPT) || echo "echo 'dpt not found'; exit 1" | sh)

SRC := flowpoll.dpt

# Define DPT_BUILD_DIR based on the absolute path of SRC and the base name of the file
DPT_BUILD_DIR := $(dir $(abspath $(SRC)))build_$(basename $(notdir $(SRC)))

OUTPUT_FILE := $(DPT_BUILD_DIR)/lucid.p4

.PHONY: build clean

build: src/$(SRC)
	sudo rm -rf $(DPT_BUILD_DIR)
	$(DPTC) src/$(SRC) --new-tofino -o $(DPT_BUILD_DIR)
	cat src/custom_tofino_control.py $(DPT_BUILD_DIR)/lucid.py > $(DPT_BUILD_DIR)/lucid_with_ctl.py
	mv $(DPT_BUILD_DIR)/lucid_with_ctl.py $(DPT_BUILD_DIR)/lucid.py
copy_p4:
	cp $(DPT_BUILD_DIR)/lucid.p4 artifact/lucid.p4

clean:
	rm -rf $(DPT_BUILD_DIR)
