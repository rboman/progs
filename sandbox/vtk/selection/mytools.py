# -*- coding: utf-8 -*-


import vtk

def saveUGrid(ugrid, filename):
    """ saves an unstructured grid into a .vtu file
    """
    writer = vtk.vtkXMLUnstructuredGridWriter()
    compressor = vtk.vtkZLibDataCompressor()
    writer.SetCompressor(compressor)
    writer.SetDataModeToAscii()
    writer.SetInputData(ugrid)
    writer.SetFileName(filename)
    writer.Write() 

def loadUGrid(filename):
    """ loads an unstructured grid from a .vtu file
    """
    reader = vtk.vtkXMLUnstructuredGridReader()
    reader.SetFileName(filename)
    reader.Update()
    return reader.GetOutput()

