import sys

f=open(sys.argv[1],"r")

signatura=dict()
palabra=''
for line in f.readlines():
	if "rel=" in line:
		l=line.split('"')
		palabra=l[1]
		if palabra in signatura.keys():
			signatura[palabra]=signatura[palabra]+1
		else:
			signatura[palabra]=1

f.close()
d=dict()
for pal in signatura.keys():
	d[pal]=float(1)/float(signatura[pal])

print d

f2=open("Disc"+sys.argv[1][0:sys.argv[1].__len__()-4]+".txt","w")
for k in d.keys():
	f2.write(k+"->"+repr(d[k])+"\n")
f2.close()
