# genmesh


import utils

par = utils.MeshParameters()
par.load('../mesh.par')
#par.save('mesh_2.par')
#par.output()

mesh = utils.Mesh()
mesher = utils.MeshBuilder(mesh)

mesher.setParameters(par)
mesher.printParameters()
mesher.genere()

mesh.output()

rnb = utils.NodeRenumberer(mesh) 

rnb.setStyle(utils.NORMALSTYLE)
rnb.execute()
writer1 = utils.OofelieMeshExporter(mesh)
writer1.save()

rnb.setStyle(utils.BACONSTYLE)
rnb.execute()
writer2 = utils.BaconMeshExporter (mesh)
writer2.save()

rnb.setStyle(utils.NORMALSTYLE)
rnb.execute()
writer3 = utils.MatlabMeshExporter (mesh)
writer3.save()




