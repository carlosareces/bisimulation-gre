import sys

numero=sys.argv[1]

f=open("GRE3D7-descriptions.xml", "r")
f2=open("res.txt","a")
f3=open("original"+numero+".txt","w")
f4=open("cleaned"+numero+".txt","w")

in_trial=0
buf=''
count=0
for line in f.readlines():
	if "<TRIAL" in line and 's="'+numero+'"' in line:
		in_trial=1
		buf=buf+line
		count=count+1
	else:
		if "</TRIAL>" in line and in_trial:
			in_trial=0
			buf=buf+line
			print buf
			buf=''
		elif in_trial:
			if "\t\t<CLEANED>" in line:
				f4.write(line.replace("\t\t<CLEANED>","").replace("</CLEANED>",""))
			elif "<ORIGINAL>" in line:
				f3.write(line.replace("\t\t<ORIGINAL>","").replace("</ORIGINAL>",""))
			buf=buf+line
f2.write("objeto: "+repr(numero)+" "+repr(count)+"\n")

f.close()
f2.close()
f3.close()
f4.close()
