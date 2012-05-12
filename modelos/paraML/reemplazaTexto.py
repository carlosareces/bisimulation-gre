import sys
from string import strip

numero=sys.argv[1]

f=open("nuevoTexto3/P_uso-"+numero+".txt", "r")
f2=open("nuevoTexto4/P_uso-"+numero+".txt", "w")

for line in f.readlines():
	line2=line.replace("left-hand", "left")
	f2.write(line2)
f2.close()
f.close()

