
check:
	../../../dpt wire.dpt

compile: 
	../../../dptc wire.dpt -o wire_build --port 28@100 --port 4@100

assemble: 
	cd wire_build && make

run: 
	cd wire_build && sudo -E make hw

run-nohup:
	nohup $(MAKE) run > wire_run.log 2>&1 &