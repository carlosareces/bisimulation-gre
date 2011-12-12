import sys
from string import strip

numero=sys.argv[1]

f=open("text-"+numero+".xml", "r")

d=dict()
in_trial=0
buf=''
for line in f.readlines():
	li=line.split(" ")
	for e in li:
		if e in d.keys():
			d[e]=d[e]+1
		else:
			d[e.strip()]=1

f.close()

for k in d.keys():
	print k +"->"+repr(d[k])
