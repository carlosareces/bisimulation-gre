import sys

numero=sys.argv[1]

f=open("cleaned-"+numero+".xml", "r")

d=dict()

in_trial=0
buf=''
for line in f.readlines():
	if "<CLEANED>" in line:
		li=line.split("<CLEANED>")[1].split("</CLEANED>")[0]
		l=li.split(" ")
		for elem in l:
			if e in d.keys():
				d[strip(e)]=d[strip(e)]+1

f.close()

for elem in d.keys():
	print elem +"->"+repr(d[elem])
