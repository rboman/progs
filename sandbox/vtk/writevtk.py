#!/usr/bin/env python
# -*- coding: latin-1 -*-
# Write a (legacy) VTK file line by line from a set of points+values.
# This program was used to build a paraview export routine in C++


# 1. create some data (points)
# ----------------------------
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


# 2. build a vtkPolyData
# ----------------------

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

#grid  = vtk.vtkUnstructuredGrid()
grid  = vtk.vtkPolyData()
grid.SetPoints(points)
#grid.SetCells(vtk.vtkVertex().GetCellType(), vertices) # ugrid
grid.SetVerts(vertices)                                 # polydata
grid.GetPointData().AddArray(scalars)
grid.GetPointData().AddArray(vectors)


# 3. save as vtk file using VTK exporter
# --------------------------------------

writer = vtk.vtkPolyDataWriter()
#writer = vtk.vtkUnstructuredGridWriter()
#writer = vtk.vtkXMLUnstructuredGridWriter()
#writer = vtk.vtkXMLPolyDataWriter()
#writer.SetCompressorTypeToNone()
#writer.SetCompressorTypeToZLib()
#writer.SetDataModeToBinary()
#writer.SetDataModeToAscii()
if version>5:
    writer.SetInputData(grid)
else:
    writer.SetInput(grid)
writer.SetFileName('output.vtk')
#writer.SetFileName('output.vtp')
writer.Write()


# 4. write the same file manually
# -------------------------------

f = open('output2.vtk','w')
f.write('# vtk DataFile Version 3.0\n')
f.write('manual output\n')
f.write('ASCII\n')
f.write('DATASET POLYDATA\n')
f.write('POINTS %d float\n' % len(pts))
for p in pts:
    f.write('%f %f %f\n' % (p[0], p[1], p[2]))
f.write('VERTICES %d %d\n' %(len(pts), 2*len(pts)))
for i in range(len(pts)):
    f.write('1 %d\n' % i)
f.write( '\n')
f.write('POINT_DATA %d\n' % len(pts) )
f.write('FIELD FieldData 2\n' )
f.write('myscalars2 1 %d float\n' % len(pts) )
for no in range(len(pts)):
    f.write('%f\n' % sdata[no])
f.write('myvectors2 3 %d float\n' % len(pts) )
for no in range(len(pts)):
    f.write('%f %f %f\n' % (vdata[no][0], vdata[no][1], vdata[no][2]))
f.close()
