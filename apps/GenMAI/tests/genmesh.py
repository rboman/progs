#!/usr/bin/env python
# -*- coding: latin-1 -*-
#
#   Copyright 2003-2017 Romain Boman
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

from genmai import *

par = MeshParameters()

inpfile = os.path.join(os.path.dirname(__file__),'../mesh.txt')
par.load(inpfile)
#par.save('mesh_2.par')
#par.output()

mesh = Mesh()
mesher = MeshBuilder(mesh)

mesher.setParameters(par)
mesher.printParameters()

mesher.genere()

mesh.output()

if 0:
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

# vtk output

import sys
if not '--nogui' in sys.argv:

    import vtk
    ugrid  = vtk.vtkUnstructuredGrid()
    points = vtk.vtkPoints()
    ugrid.SetPoints(points)

    print "converting nodes to vtk"
    for i in range(mesh.numberOfNodes()):
        points.InsertPoint(i, mesh.getNodeX(i), mesh.getNodeY(i), 0.0)
        
    print "converting elems to vtk"
    for i in range(mesh.numberOfElements()): 
        quad = vtk.vtkQuad()
        ids = quad.GetPointIds()               
        for j in range(4):    
            ids.SetId( j, mesh.getNodeNumberFromElement(i, j).getInt() )
        ugrid.InsertNextCell(quad.GetCellType(), ids)     

    # save the grid
    writer = vtk.vtkXMLUnstructuredGridWriter()
    #compressor = vtk.vtkZLibDataCompressor()
    #writer.SetCompressor(compressor)
    writer.SetCompressor(None)
    writer.SetDataModeToAscii()
    #writer.SetDataModeToBinary()
    writer.SetInputData(ugrid)
    writer.SetFileName('mesh.vtu')
    writer.Write()

    # display (DEBUG)




    mapper = vtk.vtkDataSetMapper() 
    mapper.SetInputData(ugrid)
    actor = vtk.vtkActor()
    actor.SetMapper(mapper)

    gridMapper = vtk.vtkDataSetMapper() 
    gridMapper.SetResolveCoincidentTopologyToPolygonOffset()
    gridMapper.ScalarVisibilityOff()
    gridMapper.SetInputData(ugrid)
    gridActor = vtk.vtkActor()
    gridActor.GetProperty().SetRepresentationToWireframe()
    gridActor.GetProperty().SetColor(0.1 * 2, 0.2 * 2, 0.4 * 2)
    gridActor.GetProperty().SetAmbient(1.0)
    gridActor.GetProperty().SetDiffuse(0.0)
    gridActor.GetProperty().SetSpecular(0.0)
    gridActor.SetMapper(gridMapper)


    ren = vtk.vtkRenderer()
    
    renWin = vtk.vtkRenderWindow()
    renWin.SetSize(640, 480)    
    renWin.AddRenderer(ren)
    iren = vtk.vtkRenderWindowInteractor()
    iren.SetRenderWindow(renWin)

    ren.AddActor(actor) 
    ren.AddActor(gridActor) 
    ren.ResetCamera()

    iren.Initialize()
    renWin.Render()
    iren.Start()

