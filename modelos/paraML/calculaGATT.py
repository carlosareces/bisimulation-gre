
import sys

f=open(sys.argv[1],"r")

f2=open("GATT-"+sys.argv[1],"w")

d=dict()

for line in f.readlines():
	if "    <related rel=" in line:
		pal=line.split('"')[1]
		if pal in d.keys(): d[pal]=d[pal]+1
		else: d[pal]=1

for key in d:
	numero=float(1)/float(d[key])
	f2.write(key+"->"+repr(numero)+"\n")

f.close()
f2.close()
