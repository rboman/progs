#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Write a (legacy) VTK file line by line from a set of points+values.
# This program was used to build a paraview export routine in C++

import vtk
version=vtk.vtkVersion().GetVTKMajorVersion()



def createData():
    """ 1. create some data (points)"""

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
    return pts, sdata, vdata

def createPolyData(pts, sdata, vdata):

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
    
    return grid

def saveData_LegacyVTK_vtk(pts, sdata, vdata, filename, mode='binary'):
    """ save as vtk file using VTK exporter (Legacy format)"""

    grid = createPolyData(pts, sdata, vdata)

    writer = vtk.vtkPolyDataWriter()
    if version>5:
        writer.SetInputData(grid)
    else:
        writer.SetInput(grid)

    if mode=='binary':    
        writer.SetFileTypeToBinary()
    elif mode=='ascii':
        writer.SetFileTypeToASCII()
    else:
        raise Exception('unknown mode %s (choices are binary/ascii)' % mode)

    writer.SetFileName(filename)
    writer.Write()
    print "%s written" % filename

def saveData_VTP_vtk(pts, sdata, vdata, filename, mode='binary', zlib=False):

    grid = createPolyData(pts, sdata, vdata)

    writer = vtk.vtkXMLPolyDataWriter()
    if zlib:
        writer.SetCompressorTypeToZLib()
    else:
        writer.SetCompressorTypeToNone()

    if mode=='binary':    
        writer.SetDataModeToBinary()
    elif mode=='ascii':
        writer.SetDataModeToAscii()
    elif mode=='appended': # ajoute les donnees en fin de fichier 
        writer.SetDataModeToAppended()
        writer.EncodeAppendedDataOff()	# pas de base64 encoding
    else:
        raise Exception('unknown mode %s (choices are binary/ascii/appended)' % mode)

    if version>5:
        writer.SetInputData(grid)
    else:
        writer.SetInput(grid)
    writer.SetFileName(filename)
    writer.Write()
    print "%s written" % filename


def saveData_LegacyVTK_manual(pts, sdata, vdata, filename):
    f = open(filename,'w')
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
    f.write('myvectors2 3 %d float\n' % len(pts))
    for no in range(len(pts)):
        f.write('%f %f %f\n' % (vdata[no][0], vdata[no][1], vdata[no][2]))
    f.close()
    print "%s written" % filename


if __name__ == "__main__":
    pts, sdata, vdata = createData()

    # Legacy
    saveData_LegacyVTK_vtk(pts, sdata, vdata, 'output_ascii.vtk', mode='ascii')
    saveData_LegacyVTK_vtk(pts, sdata, vdata, 'output_bin.vtk', mode='binary')
    saveData_LegacyVTK_manual(pts, sdata, vdata, 'output_ascii2.vtk')
    # XML
    saveData_VTP_vtk(pts, sdata, vdata, 'output_ascii.vtp', mode='ascii', zlib=False)
    saveData_VTP_vtk(pts, sdata, vdata, 'output_asciiz.vtp', mode='ascii', zlib=True)
    saveData_VTP_vtk(pts, sdata, vdata, 'output_bin.vtp', mode='binary', zlib=False)
    saveData_VTP_vtk(pts, sdata, vdata, 'output_binz.vtp', mode='binary', zlib=True)
    saveData_VTP_vtk(pts, sdata, vdata, 'output_app.vtp', mode='appended', zlib=False)
    saveData_VTP_vtk(pts, sdata, vdata, 'output_appz.vtp', mode='appended', zlib=True)


