#!/usr/bin/env python
# -*- coding: latin-1 -*-

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

