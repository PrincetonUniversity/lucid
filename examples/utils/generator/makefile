DPTC="../../../dptc"

build:
	$(DPTC) --port 148@40 --recirc_port 196 generator.dpt -o build
assemble:
	cd build && make
sim:
	cd build && make sim
hw:
	cd build && make hw
clean:
	sudo rm -rf build
.PHONY: build assemble sim hw clean