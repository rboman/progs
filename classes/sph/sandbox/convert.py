#!/usr/bin/env python
# -*- coding: utf-8 -*-


import vtk

reader = vtk.vtkPolyDataReader()
reader.SetFileName('build/results00000018.vtk')
reader.Update()
grid = reader.GetOutput()



writer = vtk.vtkXMLPolyDataWriter()
#writer.SetCompressorTypeToNone()
#writer.SetCompressorTypeToZLib()
writer.SetDataModeToBinary()
writer.SetDataModeToAppended()
writer.EncodeAppendedDataOff()	# pas de base64 encoding

#writer.SetDataModeToAscii()
writer.SetInputData(grid)
writer.SetFileName('output.vtp')
writer.Write()
