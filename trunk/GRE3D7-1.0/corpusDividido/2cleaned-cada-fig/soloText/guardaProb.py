import sys
from string import strip

numero=sys.argv[1]

f=open("solotext2cleaned-div-"+numero+".xml", "r")
f2=open("P_uso-"+numero+".xml", "w")


lineas=0
d=dict()
for line in f.readlines():
	lineas=lineas+1
	palabras=line.split(" ")
	for pal in palabras:
		p=strip(pal)
		if p in d.keys():
			d[p]=d[p]+1
		else:
			d[p]=1

lineas=lineas-1
f.close()
f2.write("Cantidad de lineas: "+repr(lineas)+"\n")
for k in d.keys():
	if k!='':
		
		f2.write(repr(k)+ "->"+repr(d[k])+"\n")
f2.close()

