#! /usr/bin/env python
# -*- coding: latin-1; -*-


import vtk

sphere = vtk.vtkSphere()
sphere.SetCenter(127., 127., 127.)
sphere.SetRadius(100.)

sample = vtk.vtkSampleFunction()
sample.SetImplicitFunction(sphere)
sample.SetModelBounds(0.,256.,0.,255.,0.,255.)
sample.SetSampleDimensions(10,10,10)

#contour = vtk.vtkContourFilter()
contour = vtk.vtkMarchingContourFilter() 
contour.ComputeNormalsOff()
contour.SetInput(sample.GetOutput())
contour.SetValue(0,0.0)

mapper = vtk.vtkPolyDataMapper()
mapper.SetInput(contour.GetOutput())

actor = vtk.vtkActor()
actor.SetMapper(mapper)

renderer = vtk.vtkRenderer()
renderer.AddActor(actor)

window = vtk.vtkRenderWindow()
window.AddRenderer(renderer)

interactor = vtk.vtkRenderWindowInteractor()
interactor.SetRenderWindow(window)
interactor.Initialize()
interactor.Start()


