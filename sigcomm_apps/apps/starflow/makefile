LUCID = ../../../lucid
STATS =  ../../utils/report_stats.py
BUILD_DIR = p4
src = $(wildcard *.dpt)

# compile lucid to p4 in all subdirs
$(BUILD_DIR): $(src)
	@ proj=$$(basename $$(pwd)) \
	; echo "----- building $< -> $$proj/p4/lucid.* -----" \
	; d=$$(date +%s) \
	; mkdir -p logs \
	; $(LUCID) $(src) harness.p4 $(BUILD_DIR) \
	&& echo "$$proj lucid to p4 build took $$(($$(date +%s)-d)) seconds" \
	; echo "----- done building $< -> $(basename $<)/p4/ -----" \
	; echo "run 'make tofino' to build p4 to tofino"

# compile p4 to tofino in all subdirs
tofino: $(BUILD_DIR)
	@ proj=$$(basename $$(pwd)) \
	; echo "----- building $$proj/$</lucid.* -> $$proj/$</lucid/ -----" \
	; mkdir -p logs \
	; d=$$(date +%s) \
	; $(MAKE) -C $(BUILD_DIR)/. lucid  \
	&& echo "$$proj p4 to tofino build took $$(($$(date +%s)-d)) seconds" \
	; echo "----- done building $$proj/$</lucid.* -> $$proj/$</lucid/ -----"

stats:
	@ echo "----- stats for $(src) -----" \
	; $(STATS) .

clean: 
	@rm -rf $(BUILD_DIR)