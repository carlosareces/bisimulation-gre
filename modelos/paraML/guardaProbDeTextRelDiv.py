import sys
from string import strip

numero=sys.argv[1]

f=open("nuevoTexto4/P_uso-"+numero+".txt", "r")
f2=open("nuevasProbabilidadesUso/P_uso-"+numero+".txt", "w")


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

lineas=float(lineas)
f.close()
#f2.write("Cantidad de lineas: "+repr(lineas)+"\n")
for k in d.keys():
	if k!='':
		
		f2.write(repr(k).replace("'","")+ "->"+repr(d[k]/lineas)+"\n")
f2.write("terminal->1\n")
print lineas
f2.close()

