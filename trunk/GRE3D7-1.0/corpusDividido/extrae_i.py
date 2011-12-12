import sys

numero=sys.argv[1]

f=open("GRE3D7-descriptions.xml", "r")

in_trial=0
buf=''
for line in f.readlines():
	if "<TRIAL" in line and 's="'+numero+'"' in line:
		in_numero=numero
		in_trial=1
	if "</TRIAL>" in line and in_trial:
		in_trial=0
		buf=buf+line
		print buf
	elif in_trial:
		buf=buf+line
f.close()


