#! /usr/bin/env python
# -*- coding: latin-1; -*-

import vtk

reader = vtk.vtkSTLReader()
reader.SetFileName('patte.stl')
reader.Update()

plane=vtk.vtkPlane()
plane.SetOrigin(0,30,0)
plane.SetNormal(0,1,0)


cutter = vtk.vtkClipPolyData()
cutter.SetClipFunction(plane)
cutter.SetInputConnection(reader.GetOutputPort())
cutter.Update()
cutterMapper=vtk.vtkPolyDataMapper()
cutterMapper.SetInputConnection( cutter.GetOutputPort())

#create plane actor
planeActor=vtk.vtkActor()
planeActor.GetProperty().SetColor(1.0,1,0)
planeActor.GetProperty().SetLineWidth(2)
planeActor.SetMapper(cutterMapper)




mapper = vtk.vtkPolyDataMapper()
mapper.SetInputConnection(reader.GetOutputPort())

actor = vtk.vtkActor()
actor.SetMapper(mapper)
actor.GetProperty().SetColor(0.5,1,0.5)
actor.GetProperty().SetOpacity(0.1)

# display
ren = vtk.vtkRenderer()
win = vtk.vtkRenderWindow()
win.SetSize(800, 800)
win.AddRenderer(ren)

intor = vtk.vtkRenderWindowInteractor()
intor.SetRenderWindow(win)

ren.AddActor(actor)
ren.AddActor(planeActor)
ren.SetBackground(0,0,0)



cubeAxesActor = vtk.vtkCubeAxesActor()
cubeAxesActor.SetBounds(reader.GetOutput().GetBounds())
cubeAxesActor.SetCamera(ren.GetActiveCamera())

ren.AddActor(cubeAxesActor)
ren.GetActiveCamera().Azimuth(30)
ren.GetActiveCamera().Elevation(30)
ren.ResetCamera()


massProps = vtk.vtkMassProperties()
massProps.SetInputConnection(cutter.GetOutputPort())
massProps.Update()

print 'SurfaceArea =', massProps.GetSurfaceArea()
print 'Volume =', massProps.GetVolume()
#print 'NormalizedShapeIndex =', massProps.GetNormalizedShapeIndex()


win.Render()
intor.Start()