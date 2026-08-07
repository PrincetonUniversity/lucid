
check:
	../../../dpt wire.dpt

compile: 
	../../../dptc wire.dpt -o wire_build --port 4@100 --port 44@25

assemble: 
	cd wire_build && make

run: 
	cd wire_build && sudo -E make hw
