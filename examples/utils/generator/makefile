DPTC="../../../dptc"

build:
	$(DPTC) generator.dpt -o build
assemble:
	cd build && make
sim:
	cd build && make sim
hw:
	cd build && make hw
clean:
	sudo rm -rf build
.PHONY: build assemble sim hw clean