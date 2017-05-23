#! /usr/bin/env python
# -*- coding: latin-1; -*-

import vtk

reader = vtk.vtkSTLReader()
reader.SetFileName('patte.stl')
reader.Update()

mapper = vtk.vtkPolyDataMapper()
mapper.SetInputConnection(reader.GetOutputPort())

actor = vtk.vtkActor()
actor.SetMapper(mapper)


# display
ren = vtk.vtkRenderer()
win = vtk.vtkRenderWindow()
win.AddRenderer(ren)

intor = vtk.vtkRenderWindowInteractor()
intor.SetRenderWindow(win)

ren.AddActor(actor)

win.Render()
intor.Start()