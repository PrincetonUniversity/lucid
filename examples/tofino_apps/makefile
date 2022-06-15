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
# lucid -> P4
$(COMPILE_TARGETS): lucid
	@mkdir -p $(BUILDDIR)
	$(eval TARGET := $(subst compile_,,$@)) # compile_x --> x
	../../dptc $(SOURCEDIR)/$(TARGET).dpt ip_harness.p4 ip_harness_triggers.json $(BUILDDIR)/$(TARGET)

# p4 -> tofino
$(ASSEMBLE_TARGETS):
	$(eval TARGET = $(subst assemble_,,$@)) # assemble_x --> x
	cd $(BUILDDIR)/$(TARGET); make build

# test tofino
$(TEST_TARGETS):
	$(eval TARGET = $(subst test_,,$@)) # test_x --> x
	builds/$(TARGET)/libs/analyze_model_log.py txpkts builds/$(TARGET)/run_logs/model_*	
	cd $(BUILDDIR)/$(TARGET); make test $(TESTDIR)/$(TARGET).json

$(INFO_TARGETS):
	$(eval TARGET = $(subst info_,,$@)) # info_x --> x	
	builds/$(TARGET)/libs/analyze_model_log.py txpkts builds/$(TARGET)/run_logs/model_*	

compile_all : $(COMPILE_TARGETS)
	@echo "all compiles completed"

assemble_all : $(ASSEMBLE_TARGETS)
	@echo "all assembles completed"

test_all : $(TEST_TARGETS)
	@echo "all tests completed"


.PHONY: lucid