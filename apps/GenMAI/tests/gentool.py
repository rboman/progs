#!/usr/bin/env python
# -*- coding: latin-1; -*-
# gentool


from genmai import *

par = ToolParameters() 
#thisdir = 
par.load('matrix.txt')
#par.save('matrix_2.par')

matrix = Tool()
builder = ToolBuilder(matrix)

builder.setParameters(par)
builder.printParameters()
builder.genere()

matrix.output()

writer1 = OofelieToolExporter(matrix)
writer1.save()

writer2 = BaconToolExporter(matrix)
writer2.save()

writer2b = BaconDatToolExporter(matrix)
writer2b.save()

writer3 = MatlabToolExporter(matrix)
writer3.save()



