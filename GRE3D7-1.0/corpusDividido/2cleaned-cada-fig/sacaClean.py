import sys
from string import strip

numero=sys.argv[1]

f=open("2cleaned-div-"+numero+".xml", "r")
f2=open("solotext2cleaned-div-"+numero+".xml", "w")

for line in f.readlines():
	if "<CLEANED>" in line:
		line2=line[9:line.__len__()]
		if "</CLEANED>" in line2:
			line2=line2[0:line2.__len__()-10] + "\n"
	elif "</CLEANED>" in line:
		line2=line[0:line.__len__()-11] + "\n"
	else:
		line2=line
	f2.write(line2)
f.close()
f2.close()
