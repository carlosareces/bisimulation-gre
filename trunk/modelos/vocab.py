import sys
from string import strip

l=list()
f=open("vocabulario.txt", "r")
f2=open("vocab.txt", "w")
for line in f.readlines():
	if not line.strip() in l:
		l.append(line.strip())

for elem in l:
	f2.write(elem+"\n")


f.close()
f2.close()
