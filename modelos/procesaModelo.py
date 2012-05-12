import sys

f=open(sys.argv[1],"r")

d_atr=dict()
d_rel=dict()
rel_target=dict()
vecinos_target=dict()
#f2=open("vocabulario.txt","a")


vocabulario=["bottom", "large", "blue", "ball", "next-to", "centre", "cube", "small", "left", "bellow-of", "green", "above", "terminal", "on-top", "above-of", "next-of", "left-of", "big", "medium", "right"]


in_element=0
for line in f.readlines():
	if  '  <individual id="' in line:
		key=line.split('  <individual id="')[1].split('">')[0]
		d_atr[key]=list()
		d_rel[key]=list()
		rel_target[key]=list()
		vecinos_target[key]=list()
		in_element=1
	elif '  </individual>' in line:
		in_element=0
	elif '<related rel=' in line:
		if 'to="c"' in line:
			atr=line.split('rel=')[1].split(' ')[0]
			d_atr[key].append(atr)
#			f2.write(atr+"\n")
		else:
			atr=line.split('rel=')[1].split(' ')[0]
#			f2.write(atr+"\n")
			rel_to=line.split('to="')[1].split(' ')[0]
			d_rel[key].append([atr,rel_to])
			#if key=='s1':
			rel_target[key].append(atr)
			vecinos_target[key].append(rel_to)
print d_atr
print d_rel
print rel_target

for w in vocabulario:
	f2=open(w+".txt","w")
	vecinos_con_w=0
	if 's1' in d_atr.keys() or 's1' in rel_target.keys():
		#tiene w el target?
		if 's1' in d_atr.keys():
			if w in d_atr['s1']:
				f2.write("1+\t")
			else:
				f2.write("0+\t")
		else:
			if w in rel_target['s1']:
				f2.write("1+\t")
			else:
				f2.write("0+\t")
	#cuantos vecinos del target tienen w?
	for k in vecinos_target['s1']:
		if w in d_atr[k] or w in rel_target[k]:
			vecinos_con_w=vecinos_con_w+1
	f2.write(repr(vecinos_con_w+"\t"))
	
f2.close()
#left-of
#next-to
#right-of
#on-top-of



f.close()
