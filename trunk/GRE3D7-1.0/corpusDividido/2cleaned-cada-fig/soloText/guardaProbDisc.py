import sys
from string import strip

numero=sys.argv[1]

f=open("solotext2cleaned-div-"+numero+".xml", "r")
f2=open("P_disc-"+numero+".xml", "w")


lineas=0
d=dict()
count=dict()
for line in f.readlines():
	palabras=line.split(" ")
	for pal in palabras:
		p=strip(pal)
		if p in d.keys():
			d[p]=d[p]+1/palabras.__len__()
			count[p]=count[p]+1
		else:
			d[p]=1
			count[p]=1

f.close()
for k in d.keys():
	if k!='':
		f2.write(repr(k)+ "->"+repr(float(d[k])/float(count[k]))+"\n")
f2.close()

