#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Example of "structured points" dataset
#   vtkStructuredPoints is a child class of vtkImageData

import vtk

dx = 0.2
grid = vtk.vtkStructuredPoints()
#grid = vtk.vtkImageData()
grid.SetOrigin(0.1, 0.1, 0.1) # default values
grid.SetSpacing(dx, dx, dx)
grid.SetDimensions(5, 8, 10) # number of points in each direction

array = vtk.vtkDoubleArray()
array.SetNumberOfComponents(1) # this is 3 for a vector
array.SetNumberOfTuples(grid.GetNumberOfPoints())
for i in range(grid.GetNumberOfPoints()):
    array.SetValue(i, i/2.0)
grid.GetPointData().AddArray(array)
array.SetName("my_data1")


# write structured points to disk...
writer = vtk.vtkStructuredPointsWriter()
writer.SetInputData(grid)
writer.SetFileName("points.vtk")
writer.Write()

writer = vtk.vtkXMLImageDataWriter()
writer.SetInputData(grid)
writer.SetFileName("points.vti")
writer.Write()

# display grid... (to be finished)
"""
mapper = vtk.vtkDataSetMapper()
mapper.SetInputData(grid)
#mapper.ScalarVisibilityOff()

actor = vtk.vtkActor()
actor.GetProperty().SetRepresentationToWireframe()
actor.GetProperty().SetColor(0, 0, 0)
actor.SetMapper(mapper)

ren = vtk.vtkRenderer()
ren.SetBackground(0.1, 0.2, 0.4)
ren.AddActor(actor)

window = vtk.vtkRenderWindow()
window.SetSize(800, 800)
window.AddRenderer(ren)

interactor = vtk.vtkRenderWindowInteractor()
interactor.SetRenderWindow(window)

ren.ResetCamera()
window.Render()
interactor.Start()
"""