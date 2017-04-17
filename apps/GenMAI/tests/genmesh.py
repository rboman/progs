#!/usr/bin/env python
# -*- coding: latin-1; -*-


from genmai import *

par = MeshParameters()
par.load('mesh.txt')
#par.save('mesh_2.par')
#par.output()

mesh = Mesh()
mesher = MeshBuilder(mesh)

mesher.setParameters(par)
mesher.printParameters()
mesher.genere()

mesh.output()

rnb = NodeRenumberer(mesh) 

rnb.setStyle(NORMALSTYLE)
rnb.execute()
writer1 = OofelieMeshExporter(mesh)
writer1.save()

rnb.setStyle(BACONSTYLE)
rnb.execute()
writer2 = BaconMeshExporter(mesh)
writer2.save()

rnb.setStyle(NORMALSTYLE)
rnb.execute()
writer3 = MatlabMeshExporter(mesh)
writer3.save()




