# -*- coding: utf-8 -*-
import sys

f=open(sys.argv[1],"r")

dataGuardar=''
for data in f.readlines():

	data=data.replace("á","\\'a")
	data=data.replace("é","\\'e")
	data=data.replace("í","\\'{i}")
	data=data.replace("ó","\\'o")
	data=data.replace("ú","\\'u")

        data=data.replace("Á","\\'A")
        data=data.replace("É","\\'E")
        data=data.replace("Í","\\'{I}")
        data=data.replace("Ó","\\'O")
        data=data.replace("Ú","\\'U")
 
	data=data.replace("ü",'\\"u')
	data=data.replace("ñ",'\\~n')
	dataGuardar+=data
f.close()
f=open(sys.argv[1],"w")
f.write(dataGuardar)
f.close()
