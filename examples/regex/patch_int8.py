import re
import sys,os

fn=sys.argv[1]
assert(fn.endswith('lucid.p4'))
print("Patching file: ",fn)

lines=open(fn,'r').readlines()
cnt=0

with open(fn,'w') as f:
	for l in lines:
		if (
			('RegisterAction' in l) 
			or ('f_synthesized_meta' in l)
			or ('g_synthesized_meta' in l)
			or ('cell1_local' in l)
			or ('cell1_remote' in l)
			or ('cell2_local' in l)
			or ('cell2_remote' in l)
			or ('ret_remote' in l)
			or ('resRE_' in l)
		):
			cnt+=1
			l=l.replace('bit<8>','int<8>')
			if re.search("8w\d+",l):
				l=l.replace("8w","8s")
		f.write(l)

print("Done, modified %d out of %d lines." % (cnt,len(lines)))




