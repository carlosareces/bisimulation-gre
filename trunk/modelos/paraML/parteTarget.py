import sys

f=open(sys.argv[1],"r")

f2=open("TARGET-"+sys.argv[1],"w")

in_s1=0
in_otro=0
for line in f.readlines():
	if '<individual id="s1">' in line:
		in_s1=1
	if '</individual>' in line:
		if in_otro==0:f2.write(line)
		in_otro=1
	if in_s1==1 and in_otro==0:
		f2.write(line)

f.close()
f2.close()
