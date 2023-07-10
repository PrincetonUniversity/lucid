# generic makefile for lucid programs
# jsonch 9/1/21
# setup: 
# put all lucid programs in ./src
# put all test cases in ./tests
# put the harness that all programs use in ./ip_harness.p4
# usage: 
# make compile_<program> to compile lucid -> P4
# make assemble_<program> to assemble P4 -> tofino
# make test_<program> to test <program> on the tofino model at <tests/program.json>

ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
SOURCEDIR := "$(ROOT_DIR)/src"
BUILDDIR := "$(ROOT_DIR)/builds"
TESTDIR := "$(ROOT_DIR)/tests"
SOURCES := $(shell find $(SOURCEDIR) -name '*.dpt')

HARNESS := $(SOURCEDIR)/ip_harness.p4

COMPILE_OLD_TARGETS := $(addprefix old_compile_,$(strip $(basename $(notdir $(SOURCES)))))
COMPILE_TARGETS := $(addprefix compile_,$(strip $(basename $(notdir $(SOURCES)))))
ASSEMBLE_TARGETS := $(addprefix assemble_,$(strip $(basename $(notdir $(SOURCES)))))
TEST_TARGETS := $(addprefix test_,$(strip $(basename $(notdir $(SOURCES)))))
INFO_TARGETS := $(addprefix info_,$(strip $(basename $(notdir $(SOURCES)))))

lucid: 
	{ \
	cd ../../ ;\
	make ;\
	cd - ;\
	}

$(COMPILE_OLD_TARGETS): lucid
	@mkdir -p $(BUILDDIR)
	$(eval TARGET := $(subst old_compile_,,$@)) # compile_x --> x
	../../dptc $(SOURCEDIR)/$(TARGET).dpt -o $(BUILDDIR)/$(TARGET)

# lucid -> P4
$(COMPILE_TARGETS): lucid
	@mkdir -p $(BUILDDIR)
	$(eval TARGET := $(subst compile_,,$@)) # compile_x --> x
	../../dptc --new-tofinocore $(SOURCEDIR)/$(TARGET).dpt -o $(BUILDDIR)/$(TARGET)

# p4 -> tofino
$(ASSEMBLE_TARGETS):
	$(eval TARGET = $(subst assemble_,,$@)) # assemble_x --> x
	cd $(BUILDDIR)/$(TARGET); make build

# test tofino
$(TEST_TARGETS):
	$(eval TARGET = $(subst test_,,$@)) # test_x --> x
	cd $(BUILDDIR)/$(TARGET); make test $(TESTDIR)/$(TARGET).json
	builds/$(TARGET)/libs/validationutils.py txpkts builds/$(TARGET)/run_logs/model_*	

$(INFO_TARGETS):
	$(eval TARGET = $(subst info_,,$@)) # info_x --> x	
	builds/$(TARGET)/libs/validationutils.py txpkts builds/$(TARGET)/run_logs/model_*	

compile_all : $(COMPILE_TARGETS)
	@echo "all compiles completed"

assemble_all : $(ASSEMBLE_TARGETS)
	@echo "all assembles completed"

test_all : $(TEST_TARGETS)
	@echo "all tests completed"


.PHONY: lucid
