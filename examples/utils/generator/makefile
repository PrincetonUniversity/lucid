BUILD_DIR := build
build:
	@if [ -z "$(LDE)" ]; then \
		echo "LDE (the directory where the lucid compiler binary, \"dpt\", is located) is not set"; \
		exit 1; \
	fi
	mkdir -p $(BUILD_DIR)
	sudo chown -R $$USER $(BUILD_DIR)
	$(LDE)/dptc --port 148@40 --recirc_port 196 generator.dpt -o $(BUILD_DIR)

control:
	$(BUILD_DIR)/libs/link_control.sh ./generator_control.py $(BUILD_DIR)/lucid.py 
assemble:
	cd $(BUILD_DIR) && make
sim:
	cd $(BUILD_DIR) && make sim
start:
	cd $(BUILD_DIR) && sudo SDE=$(SDE) SDE_INSTALL=$(SDE_INSTALL) make hw &	
stop: 
	sudo killall bf_switchd

package:
	# package build dir
	zip -r $(BUILD_DIR).zip $(BUILD_DIR)

unpackage:
	# unzip the package on the target machine
	unzip $(BUILD_DIR).zip
	# update the conf file paths
	./$(BUILD_DIR)/libs/updateconfpaths.py $(BUILD_DIR)/lucid_tofino/lucid.conf

clean:
	sudo rm -rf $(BUILD_DIR)

reflector: reflector.cpp
	g++ reflector.cpp -o reflector -lpcap -std=c++11

.PHONY: $(BUILD_DIR) assemble sim clean start stop