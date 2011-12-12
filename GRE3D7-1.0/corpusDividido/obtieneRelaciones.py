import sys
from string import strip

numero=sys.argv[1]

f=open("fig-"+numero+".xml", "r")

f3=open("relaciones.txt","r")

relaciones=dict()
for line in f3.readlines():
	l=line.strip()
	if not l in relaciones.keys():
		relaciones[l] = 1
f3.close()

for line in f.readlines():
	if "<CLEANED>" in line:
		texto = line
	if "<REORDERED_PATTERN>" in line:
		if "rel" in line:
			lista_pal = line.split(" ")
			n = 0
			for l in lista_pal:
				if l=="rel":
					pos = n
				n = n+1
			guardar = 1
	else:
		guardar = 0
	if guardar:
		rel=texto.split(" ")[pos]
		if rel not in relaciones.keys():
			relaciones[rel] = 1
		else:
			relaciones[rel] = relaciones[rel]+1

f.close()
f2 = open("relaciones.txt", "w")

for k in relaciones.keys():
	f2.write(k+"\n")
f2.close()

