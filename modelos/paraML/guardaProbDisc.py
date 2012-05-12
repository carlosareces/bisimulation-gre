import sys
from string import strip

numero=sys.argv[1]

f=open("cleaned-div/text-rel-div-"+numero+".txt", "r")
f2=open("P_disc/P_disc-"+numero+".txt", "w")


lineas=0
d=dict()
count=dict()
for line in f.readlines():
	palabras=line.split(" ")
	for pal in palabras:
		p=strip(pal)
		if p in d.keys():
			d[p]=d[p]+float(1.00/float(palabras.__len__()))
			count[p]=count[p]+1
			#print "Sumo: %s - %f - %d"%(p, d[p], count[p])
		else:
			d[p]=float(1.00/float(palabras.__len__()))
			count[p]=1

f.close()
for k in d.keys():
	if k!='':
		f2.write(repr(k).replace("'","")+ "->"+repr(float(d[k])/float(count[k]))+"\n")
f2.close()

