import sys

f=open(sys.argv[1],"r")
f2=open("vecinos-"+sys.argv[1],"w")

for line in f.readlines():
	if 'to=' in line and not 'to="c"' in line:
		vecino=line.split('"')[3] 
		f2.write(vecino+"\n")

f.close()
f2.close()
