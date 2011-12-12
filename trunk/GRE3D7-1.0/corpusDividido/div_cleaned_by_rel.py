import sys
from string import strip

numero=sys.argv[1]

f=open("cleaned-"+numero+".xml", "r")

f3=open("cleaned-div-"+numero+".xml", "w")
relaciones=open("relaciones.txt","r")
l=list()
for line in relaciones.readlines():
	l.append(line.strip())
print l
for line in f.readlines():
	agrego=0
	for pal in l:
		if pal in line:
#		<CLEANED>small green ball left-of large blue cube</CLEANED>
			li_div = line.split(pal)
			f3.write(li_div[0]+pal+"\n")
			f3.write(pal+li_div[1])
			agrego=1
			break
	if not agrego:
		f3.write(line)
	
f.close()
f3.close()

f4=open("cleaned-div-"+numero+".xml","r")
f5=open("2cleaned-div-"+numero+".xml","w")

for line in f4.readlines():
	if "\t\t" in line:
		l=line.split("\t\t")
		line2=l[1]
	else:
		line2=line
	f5.write(line2)

