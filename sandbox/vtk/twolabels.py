#! /usr/bin/env python
# -*- coding: latin-1 -*-
#
#   Copyright 2017 Romain Boman
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

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
# <= move the labels one space char to the right (not mandatory)
lmapper1.SetLabelFormat(" %d")

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
p.SetColor(0, 1, 1)
# <= move the labels one space char to the left (not mandatory)
lmapper2.SetLabelFormat("%d ")

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
