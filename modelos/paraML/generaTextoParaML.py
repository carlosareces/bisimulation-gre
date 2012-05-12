#!/usr/bin/python

from string import strip
import sys
from string import replace, strip
f=open("vocabP_use.txt","r")

NOUN=["cube", "ball"]
ADJ=["large","small", "blue", "green","left","right","centre"]
REL=["above-of", "on-top", "right-of", "left-of"]

l=[1, 3, 6 ,8, 10, 12, 13, 15, 18, 20, 23, 25, 27, 30, 32]

for line in f.readlines():
	palabra=line.strip()
	f2=open("archivosParaML/"+palabra+".txt","w") #archivo en el que escribo
	f2.write("@relation probabilidad-uso\n\n")
	f2.write("@attribute target-tiene numeric {0,1}\n")
##--------------------------------Agregue estos
	f2.write("@attribute cant-prop-rel integer\n")
	f2.write("@attribute noun-target-tiene integer\n")
	f2.write("@attribute adj-target-tiene integer\n")
	f2.write("@attribute rel-target-tiene integer\n")
##---------------------------------------------------------
	f2.write("@attribute vecino-tiene numeric {0,1}\n")
##	f2.write("@attribute disc REAL\n")
	f2.write("@attribute disc-gatt REAL\n")
	f2.write("@attribute prob-uso REAL\n\n")
	f2.write("@data\n")
	for i in l:
		f3=open("PAL-TARGET-modeloFig"+repr(i)+"Gre7-2.xml", "r")
		tiene=0
		for line2 in f3.readlines():
			if line2.strip()==palabra:
				tiene=1 
		if tiene==1: f2.write("1,")
		else: f2.write("0,")
		f3.close()
		##------hasta aca target-tiene
		f9=open("PAL-TARGET-modeloFig"+repr(i)+"Gre7-2.xml")
		noun=0
		adj=0
		rel=0
		for line9 in f9.readlines():
			o=line9.strip()
			if o in NOUN: noun=noun+1
			elif o in ADJ: adj=adj+1
			elif o in REL: rel=rel+1
			else: print "palabra que no estaba: "+ o +" en modelo: ", repr(i)
		f2.write(repr(noun+adj+rel)+","+repr(noun)+","+repr(adj)+","+repr(rel)+",")
		##------hasta aca cant-prop-rel target tiene noun adj rel
		f4=open("s2-PAL-s2-modeloFig"+repr(i)+"Gre7-2.xml", "r")
		tiene=0
		for line2 in f4.readlines():
			if line2.strip()==palabra:
				tiene=1 
		if tiene==1: f2.write("1,")
		else: f2.write("0,")
		f4.close()
		##-------hasta aca vecino-tiene
		#f5=open("P_disc-"+repr(i)+".txt","r")
		#prob=0.00
		#for line2 in f5.readlines():
		#	if palabra in line2:
		#		prob=line2.split("->")[1].strip()
		#f2.write(repr(prob)+",")
		#f5.close()
		f6=open("GATT-modeloFig"+repr(i)+"Gre7-2.xml","r")
		prob=0.00
		for line2 in f6.readlines():
			if palabra in line2:
				prob=line2.split("->")[1].strip()
		f2.write(repr(prob)+",")
		f6.close()
		f7=open("P_uso-"+repr(i)+".txt","r")
		prob=0.00
		for line2 in f7.readlines():
			if palabra in line2:
				prob=line2.split("->")[1].strip()
		f2.write(repr(prob)+"\n")
		f7.close()
	f2.close()
	f2=open("archivosParaML/"+palabra+".txt","r")
	f8=open("archivosParaML2/"+palabra+".arff","w")
	for line2 in f2.readlines():
		if "'" in line2:
			line3=line2.replace("'","")
		else: line3=line2
		f8.write(line3)
	f8.close()
f.close()

