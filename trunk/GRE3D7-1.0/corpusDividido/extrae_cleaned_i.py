import sys

numero=sys.argv[1]

f=open("fig-"+numero+".xml", "r")

in_trial=0
buf=''
for line in f.readlines():
	if "<CLEANED>" in line or "<ORIG_PATTERN>" in line or "<REORDERED_PATTERN>" in line:
		buf=buf+line
print buf
f.close()

