#!/usr/bin/env python
# -*- coding: utf-8 -*-

import vtk
import numpy as np
from vtk.util import numpy_support

px = np.linspace(0.0, 10.0, 11)
py = np.linspace(0.0, 5.0, 6)
pz = np.linspace(0.0, 8.0, 9)
print px.size

rgrid = vtk.vtkRectilinearGrid()
rgrid.SetDimensions(px.size, py.size, pz.size);
rgrid.SetXCoordinates(numpy_support.numpy_to_vtk(px))
rgrid.SetYCoordinates(numpy_support.numpy_to_vtk(py))
rgrid.SetZCoordinates(numpy_support.numpy_to_vtk(pz))

print numpy_support.numpy_to_vtk(px)

mapper = vtk.vtkDataSetMapper()
mapper.SetInputData(rgrid)
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
