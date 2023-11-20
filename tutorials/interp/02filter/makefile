# lucid.sh is a wrapper to the dockerized lucid build system.
FIND_LUCID_SH := ./findfile.sh lucid.sh
LUCID_SH := $(shell $(FIND_LUCID_SH) || echo "echo 'lucid.sh not found'; exit 1" | sh)

# "dpt" is the lucid interpreter binary. This is for if you have a 
# local build on your machine, or if you ssh into the docker 
# image and want to run the makefile from there.
FIND_DPT := ./findfile.sh dpt
DPT := $(shell $(FIND_DPT) || echo "-" )


# SRC is the lucid source program. It can be passed 
# as an argument to this makefile, or as a default 
# value the first .dpt file found is used.
ifndef SRC
SRC := $(firstword $(wildcard *.dpt))
endif

.PHONY: interp interp_local build_p4 clean dptc force current_makefile

# run the lucid interpreter on SRC, using the dockerized lucid.sh script.
interp:
	@$(LUCID_SH) interp $(SRC)

# run the lucid interpreter on SRC, using the local build 
# of the lucid interpreter (which is named dpt)
interp_local:
	@if [ "$(DPT)" = "-" ]; then \
		echo "local dpt not found. falling back to docker"; \
		$(LUCID_SH) interp $(SRC); \
	else \
		$(DPT) $(SRC); \
	fi

# everything below is related to compiling 
# lucid to P4-tofino. Ignore if you are only  
# using the interpeter. 

# P4_OUTPUT is the lucid-generate p4 program. 
P4_OUTPUT := $(DPT_BUILD_DIR)/lucid.p4


build_p4: $(P4_OUTPUT)

$(P4_OUTPUT): $(SRC)
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
