#!/usr/bin/python
from os import *

l=[1, 3, 6 ,8, 10, 12, 13, 15, 18, 20, 23, 25]
for i in l:
    system('python palabrasTarget.py VECINO-modeloFig'+repr(i)+'Gre7-2.xml')
