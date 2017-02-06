#!/usr/bin/env python
# -*- coding: latin-1; -*-





# create data

pts = []
pts.append( (0.0, 0.0, 0.0) )
pts.append( (1.0, 0.0, 0.0) )
pts.append( (1.0, 1.0, 0.0) )
pts.append( (0.0, 1.0, 0.0) )
pts.append( (0.0, 0.0, 1.0) )
pts.append( (1.0, 0.0, 1.0) )
pts.append( (1.0, 1.0, 1.0) )
pts.append( (0.0, 1.0, 1.0) )

sdata = []
for no, p in enumerate(pts):
    sdata.append(float(no))

vdata = []
for no, p in enumerate(pts):
    vdata.append( ((p[0]-0.5)/2, (p[1]-0.5)/2, (p[2]-0.5)/2) )


# save as vtk file

import vtk
version=vtk.vtkVersion().GetVTKMajorVersion()

points = vtk.vtkPoints()

vertices = vtk.vtkCellArray()

scalars = vtk.vtkFloatArray()
scalars.SetNumberOfComponents(1)
scalars.SetName('myscalars')

vectors = vtk.vtkFloatArray()
vectors.SetNumberOfComponents(3)
vectors.SetName('myvectors')

for no, p in enumerate(pts):
    points.InsertNextPoint(p[0], p[1], p[2])
    vertex = vtk.vtkVertex()
    vertex.GetPointIds().SetId(0,no)
    vertices.InsertNextCell(vertex)
    scalars.InsertNextValue(sdata[no])
    vectors.InsertNextTuple3(vdata[no][0], vdata[no][1], vdata[no][2])






grid  = vtk.vtkUnstructuredGrid()
grid.SetPoints(points)
grid.SetCells(vtk.vtkVertex().GetCellType(), vertices)
grid.GetPointData().AddArray(scalars)
grid.GetPointData().AddArray(vectors)


#writer = vtk.vtkUnstructuredGridWriter()
writer = vtk.vtkXMLUnstructuredGridWriter()
writer.SetCompressorTypeToNone()
#writer.SetCompressorTypeToZLib()
#writer.SetDataModeToBinary()
writer.SetDataModeToAscii()
if version>5:
    writer.SetInputData(grid)
else:
    writer.SetInput(grid)
#writer.SetFileName('output.vtk')
writer.SetFileName('output.vtu')
writer.Write()



