import sys
from string import strip

numero=sys.argv[1]

f=open("PAL-TARGET-modeloFig"+numero+"Gre7-2.xml","r")

for line in f.readlines():
	if "-" in line:
		relacion=line.strip()
		break
f2=open("P_uso-"+numero+".txt","r")

f3=open("nuevaP_use/P_uso-"+numero+".txt","w")

for line2 in f2.readlines():
	line3=line2.replace("next-to", relacion)
	f3.write(line3)
f.close()
f2.close()
f3.close()
