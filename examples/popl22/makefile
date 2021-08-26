# the general command to build is: 
# dptc <app.dpt> <harness.p4> <build_dir>
# Each sigcomm app has a custom template. 
# to copy the template for <app>.dpt as <app>.p4 in the local directory, 
# use: ./scripts/cp_harness.sh <app>
# to get the version of <app>.dpt that's in the sigcomm 
# artifact, use: ./scripts/cp_original_app.sh <app>
DPTC = ../../dptc
BUILD_DIR = "build"

compiler: 
	cd ../../; make; cd -

# compiles 
rerouter: rerouter.dpt
	$(DPTC) $@.dpt $@.p4 $(BUILD_DIR)/$@

# compiles
dnsguard: dnsguard.dpt
	$(DPTC) $@.dpt $@.p4 $(BUILD_DIR)/$@

# looks like it would compile, but would take 30 minutes
starflow: starflow.dpt
	$(DPTC) $@.dpt $@.p4 $(BUILD_DIR)/$@

# compiles
NAT: NAT.dpt
	$(DPTC) $@.dpt $@.p4 $(BUILD_DIR)/$@

# compiles
chain_prob_stateful_firewall: chain_prob_stateful_firewall.dpt
	$(DPTC) $@.dpt $@.p4 $(BUILD_DIR)/$@

# compiles
countmin_historical: countmin_historical.dpt
	$(DPTC) $@.dpt $@.p4 $(BUILD_DIR)/$@

# compiles
chain_replication: chain_replication.dpt
	$(DPTC) $@.dpt $@.p4 $(BUILD_DIR)/$@

# compiles
chain_prob_stateful_firewall_timeout: chain_prob_stateful_firewall_timeout.dpt
	$(DPTC) $@.dpt $@.p4 $(BUILD_DIR)/$@

# ????
stateful_fw: stateful_fw.dpt
	$(DPTC) $@.dpt $@.p4 $(BUILD_DIR)/$@

# missing: RIP -- probably not complex enough to matter? 

clean: 
	rm -rf $(BUILD_DIR)/*