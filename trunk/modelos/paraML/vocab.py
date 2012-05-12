from string import strip

l=[1, 3, 6 ,8, 10, 12, 13, 15, 18, 20, 23, 25, 27, 30, 32]

lista=list()

for i in l:
	f=open("P_uso-"+repr(i)+".txt", "r")
	for line in f.readlines():
		if "->" in line:
			pal=line.split("->")[0]
			if not pal in lista:
				lista.append(pal)
	f.close()

f3=open("vocabP_use.txt","w")
for a in lista:
	f3.write(a+"\n")
f3.close()
