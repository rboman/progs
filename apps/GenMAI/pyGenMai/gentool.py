# gentool


import utils

par = utils.ToolParameters() 
par.load('../matrix.par')
#par.save('matrix_2.par')

matrix = utils.Tool()
builder = utils.ToolBuilder(matrix)

builder.setParameters(par)
builder.printParameters()
builder.genere()

matrix.output()

writer1 = utils.OofelieToolExporter (matrix)
writer1.save()

writer2 = utils.BaconToolExporter (matrix)
writer2.save()

writer2b = utils.BaconDatToolExporter (matrix)
writer2b.save()

writer3 = utils.MatlabToolExporter (matrix)
writer3.save()



