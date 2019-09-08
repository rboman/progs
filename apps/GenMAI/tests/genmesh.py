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




mesh = Mesh()
mesher = MeshBuilder(mesh)
if 0:
    mesher.origin.x = -10.
    mesher.origin.y = -0.25875
    mesher.dimension.x = 10.
    mesher.dimension.y = 0.25875
    mesher.numberOfElementOnX = 200
    mesher.numberOfElementOnY = 2
    mesher.reductionCoefficient = 5.0
    mesher.layers.clear()
    mesher.layers.push_back(REDUCTION)
    mesher.layers.push_back(REDUCTION)
    mesher.layers.push_back(REDUCTION)
    mesher.layers.push_back(REDUCTION)
    mesher.layers.push_back(REDUCTION)
    mesher.layers.push_back(REDUCTION)
    mesher.layers.push_back(CONSTANT)

mesher.genere()

print mesh

# vtk output

import sys
if not '--nogui' in sys.argv:

    import vtk
    ugrid  = vtk.vtkUnstructuredGrid()
    points = vtk.vtkPoints()
    ugrid.SetPoints(points)

    print "converting nodes to vtk"
    for i in xrange(mesh.numberOfNodes()):
        points.InsertPoint(i, mesh.getNodeX(i), mesh.getNodeY(i), 0.0)
        
    print "converting elems to vtk"
    for i in xrange(mesh.numberOfElements()): 
        quad = vtk.vtkQuad()
        ids = quad.GetPointIds()               
        for j in range(4):    
            ids.SetId( j, mesh.getNodeNumberFromElement(i, j) )
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

