import sys

f=open(sys.argv[1],"r")

f2=open("s2-"+sys.argv[1],"w")

in_s2=0

primero=0
for line in f.readlines():
	if '<individual id="s2">' in line:
		in_s2=1
	if '</individual>' in line:
		primero=primero+1
		if primero==2:
			f2.write(line)
	if in_s2==1 and primero<2:
		f2.write(line)

f.close()
f2.close()
