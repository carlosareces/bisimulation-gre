import sys
from string import strip

numero=sys.argv[1]

f=open("solotext2cleaned-div-"+numero+".xml","r")

d_pal=dict()
num_lineas=0
for line in f.readlines():
	num_lineas=num_lineas+1
	pal=line.split(" ")
	for p in pal:
		if p.strip() in d_pal.keys():
			d_pal[p.strip()]=d_pal[p.strip()]+1
		else:
			d_pal[p.strip()]=1

f.close()
f2=open("frec_pal-"+numero+".txt", "w")

f2.write(num_lineas+"\n")

for k in d_pal.keys():
	f2.write(k+"->"+d_pal[k]+"\n")
f2.close()




