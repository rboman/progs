
#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import vtk

reader = vtk.vtkXMLPolyDataReader()
reader.SetFileName('geo_layers.vtp')
reader.Update()

mapper = vtk.vtkPolyDataMapper()
mapper.SetInputConnection(reader.GetOutputPort())

actor = vtk.vtkActor()
actor.SetMapper(mapper)

# prp = actor.GetProperty()
# prp.SetLineWidth(2)



ren = vtk.vtkRenderer()
ren.SetBackground(0.1, 0.2, 0.4)
ren.AddActor(actor)

renWin = vtk.vtkRenderWindow()
renWin.SetSize(640, 480)
renWin.AddRenderer(ren)
renWin.Render()

iren = vtk.vtkRenderWindowInteractor()
iren.SetRenderWindow(renWin)
iren.Initialize()
iren.Start()