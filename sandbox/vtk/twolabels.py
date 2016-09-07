#! /usr/bin/env python
# -*- coding: latin-1; -*-

import vtk

# generate some points
psource = vtk.vtkPointSource()
psource.SetNumberOfPoints(10)
psource.Update()

pmapper = vtk.vtkPolyDataMapper()
pmapper.SetInputConnection(psource.GetOutputPort())

pactor = vtk.vtkActor()
pactor.SetMapper(pmapper)
pactor.GetProperty().SetPointSize(8)
pactor.GetProperty().SetColor(1, 1, .4)

# labels #1
lmapper1 = vtk.vtkLabeledDataMapper()
lmapper1.SetInputConnection(psource.GetOutputPort())
lmapper1.SetLabelFormat(" %d") # <= move the labels one space char to the right (not mandatory)

lactor1 = vtk.vtkActor2D()
lactor1.SetMapper(lmapper1)

# labels #2
lmapper2 = vtk.vtkLabeledDataMapper()
lmapper2.SetInputConnection(psource.GetOutputPort())
 
lactor2 = vtk.vtkActor2D()
lactor2.SetMapper(lmapper2)

p = lmapper2.GetLabelTextProperty()
p.SetJustificationToRight()         # <= right justification
p.SetVerticalJustificationToTop()   # <= top justification
p.SetColor(0,1,1)
lmapper2.SetLabelFormat("%d ")      # <= move the labels one space char to the left (not mandatory)

# display
ren = vtk.vtkRenderer()
win = vtk.vtkRenderWindow()
win.AddRenderer(ren)

intor = vtk.vtkRenderWindowInteractor()
intor.SetRenderWindow(win)

ren.AddActor(pactor)
ren.AddActor(lactor1)
ren.AddActor(lactor2)
 
win.Render()
intor.Start()
