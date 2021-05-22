TOPTARGETS := clean p4 tofino stats
SUBDIRS := $(wildcard apps/*/.)

$(TOPTARGETS): $(SUBDIRS)
$(SUBDIRS):
		@$(MAKE) --no-print-directory -C $@ $(MAKECMDGOALS)

.PHONY: $(TOPTARGETS) $(SUBDIRS)