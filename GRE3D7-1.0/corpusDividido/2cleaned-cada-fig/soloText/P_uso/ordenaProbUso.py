import sys
from string import strip, atoi

numero=sys.argv[1]

f=open("P_uso-"+numero+".xml", "r")
f2=open("OrdenadaP_uso-"+numero+".xml", "w")

line=f.readline()#leo primera linea y la escribo nomas
f2.write(line)

d=dict()
l=list()
for line in f.readlines():
	palabras=line.split("->")
	pal=palabras[0], 
	num=atoi(palabras[1].strip())
	if palabras.__len__()==2:
		d[num]=pal

a=list()
a=d.keys()
a.sort()
for k in a:
	b=d[k]
	f2.write("%s->%i\n"%(b[0],k))

f.close()
f2.close()

