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

par = ToolParameters() 
inpfile = os.path.join(os.path.dirname(__file__),'../matrix.txt')
par.load(inpfile)
#par.save('matrix_2.par')

matrix = Tool()
builder = ToolBuilder(matrix)

builder.setParameters(par)
builder.printParameters()
builder.genere()

matrix.output()

if 0:
    writer1 = OofelieToolExporter(matrix)
    writer1.save()
    
    writer2 = BaconToolExporter(matrix)
    writer2.save()
    
    writer2b = BaconDatToolExporter(matrix)
    writer2b.save()
    
    writer3 = MatlabToolExporter(matrix)
    writer3.save()



# vtk output
import sys
if not '--nogui' in sys.argv:
    import vtk
    ugrid  = vtk.vtkUnstructuredGrid()
    points = vtk.vtkPoints()
    ugrid.SetPoints(points)

    print "converting points to vtk"
    for i in range(matrix.getFirstPoint(), matrix.numberOfPoints()):
        points.InsertPoint(i-matrix.getFirstPoint(), matrix.getPointX(i), matrix.getPointY(i), 0.0)
        
    print "converting curves to vtk"
    for i in range(matrix.getFirstCurve(), matrix.numberOfCurves()-1):  # ! shift!!
        nbp = matrix.getCurve(i).numberOfPoints()
        if nbp==2:
            cell = vtk.vtkLine() 
            cell.GetPointIds().SetId( 0, matrix.getCurve(i).getPointNumber(0) -matrix.getFirstPoint()-1)
            cell.GetPointIds().SetId( 1, matrix.getCurve(i).getPointNumber(1) -matrix.getFirstPoint()-1)
        elif nbp==3:
            cell = vtk.vtkQuadraticEdge() 
            cell.GetPointIds().SetId( 0, matrix.getCurve(i).getPointNumber(0) -matrix.getFirstPoint()-1)
            cell.GetPointIds().SetId( 1, matrix.getCurve(i).getPointNumber(2) -matrix.getFirstPoint()-1)
            cell.GetPointIds().SetId( 2, matrix.getCurve(i).getPointNumber(1) -matrix.getFirstPoint()-1)    
        else:
            raise Exception ("curve with %d points" % nbp)

        
        ugrid.InsertNextCell(cell.GetCellType(), cell.GetPointIds() )     

    # save the grid
    writer = vtk.vtkXMLUnstructuredGridWriter()
    #compressor = vtk.vtkZLibDataCompressor()
    #writer.SetCompressor(compressor)
    writer.SetCompressorTypeToNone()
    writer.SetDataModeToAscii()
    writer.SetInputData(ugrid)
    writer.SetFileName('tool.vtu')
    writer.Write()


    # display (DEBUG)

    mapper = vtk.vtkDataSetMapper() 
    mapper.SetInputData(ugrid)
    actor = vtk.vtkActor()
    actor.SetMapper(mapper)

    ren = vtk.vtkRenderer()
    
    renWin = vtk.vtkRenderWindow()
    renWin.SetSize(640, 480)    
    renWin.AddRenderer(ren)
    iren = vtk.vtkRenderWindowInteractor()
    iren.SetRenderWindow(renWin)

    ren.AddActor(actor) 
    ren.ResetCamera()

    iren.Initialize()
    renWin.Render()
    iren.Start()

