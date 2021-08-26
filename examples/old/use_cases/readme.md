### Use case examples

#### Data structures

*Simple key value store*
	- lookup at packetin
	- insert at insert event

*Best port selection*
	pkt_in:
	- get a set of paths for a key
	- read the load of each path
	- select the least loaded path
	background: 
	- update path load in both arrays


*Telemetry batching*
	pkt_in: 
	have N arrays, in sequence
	write one packet to each array
	when we have written N arrays, generate the report
