#! /usr/bin/env python
# -*- coding: latin-1; -*-

import vtk
version=vtk.vtkVersion().GetVTKMajorVersion()

# data

reader = vtk.vtkPolyDataReader()
reader.SetFileName('brain.vtk')
reader.Update()

# stupid filter

shrink = vtk.vtkShrinkPolyData()
if version>5:
    shrink.SetInputData(reader.GetOutput())
else:
    shrink.SetInput(reader.GetOutput())
shrink.SetShrinkFactor(0.8)

# actor 1

mapper = vtk.vtkPolyDataMapper()
mapper.SetInputConnection(shrink.GetOutputPort())
actor = vtk.vtkActor()
actor.GetProperty().SetColor(1.0,1.0,1.0)
actor.SetMapper(mapper)

# actor 2

mapper2 = vtk.vtkPolyDataMapper()
mapper2.SetInputConnection(reader.GetOutputPort())
actor2 = vtk.vtkActor()
actor2.SetMapper(mapper2)
actor2.GetProperty().SetOpacity(0.8)
actor2.GetProperty().SetAmbient(0.5)
actor2.GetProperty().SetDiffuse(0.5)
actor2.GetProperty().SetSpecular(1.0)
actor2.GetProperty().SetSpecularPower(10.0)
actor2.SetScale(1.2,1.2,0.8)
actor2.AddPosition(100,0,0)
actor2.GetProperty().SetColor(1.0,0.0,0.0)
actor2.RotateX(90)
actor2.RotateZ(90)

# actor 3

#gen = vtk.vtkTextureMapToPlane()
gen = vtk.vtkTextureMapToCylinder()
#gen = vtk.vtkTextureMapToSphere()
#gen.PreventSeamOn()
gen.SetInputConnection(reader.GetOutputPort())
#gen.PreventSeamOn()

xform = vtk.vtkTransformTextureCoords()
xform.SetInputConnection(gen.GetOutputPort())
xform.SetScale(3,-3,0)


bmpReader = vtk.vtkBMPReader()
bmpReader.SetFileName('bouteille.bmp')
texture = vtk.vtkTexture()
texture.SetInputConnection(bmpReader.GetOutputPort())
texture.InterpolateOn()

mapper3 = vtk.vtkPolyDataMapper()
mapper3.SetInputConnection(xform.GetOutputPort())
actor3 = vtk.vtkActor()
actor3.SetMapper(mapper3)
actor3.SetTexture(texture)
actor3.AddPosition(-200,0,0)
actor3.GetProperty().SetOpacity(1.0)
actor3.GetProperty().SetAmbient(0.0)
actor3.GetProperty().SetDiffuse(1.0)


# renderer ---

ren = vtk.vtkRenderer()
ren.SetBackground(0.1, 0.2, 0.4) 
       
renWin = vtk.vtkRenderWindow()
renWin.SetSize(640, 480)    
renWin.AddRenderer(ren)
iren = vtk.vtkRenderWindowInteractor()
iren.SetRenderWindow(renWin)

ren.AddActor(actor)
ren.AddActor(actor2)
ren.AddActor(actor3)

cam1 = vtk.vtkCamera()
cam1.ParallelProjectionOn()
cam1.SetParallelScale(230)
cam1.SetClippingRange(0,10000000)
cam1.SetFocalPoint(125,125,100)
cam1.SetPosition(-4000,0,0)
#cam1.ComputeViewPlaneNormal()
cam1.SetViewUp(0,0,1)
ren.SetActiveCamera(cam1)


light = vtk.vtkLight()
light.SetColor(1,1,1)
light.SetFocalPoint(cam1.GetFocalPoint())
light.SetPosition(cam1.GetPosition())

ren.AddLight(light)

print cam1

# axes a la paraview
axes = vtk.vtkAxesActor()
axes.SetShaftTypeToCylinder()
axes.SetXAxisLabelText("x")
axes.SetYAxisLabelText("y")
axes.SetZAxisLabelText("z")
axes.SetTotalLength(1, 1, 1)
tprop = vtk.vtkTextProperty()
tprop.ItalicOn()
#tprop.ShadowOn()
#tprop.SetFontFamilyToTimes()
axes.GetXAxisCaptionActor2D().SetCaptionTextProperty(tprop)
tprop2 = vtk.vtkTextProperty()
tprop2.ShallowCopy(tprop)
axes.GetYAxisCaptionActor2D().SetCaptionTextProperty(tprop2)
tprop3 = vtk.vtkTextProperty()
tprop3.ShallowCopy(tprop)
axes.GetZAxisCaptionActor2D().SetCaptionTextProperty(tprop3)

marker = vtk.vtkOrientationMarkerWidget()
#marker.SetOutlineColor(0.93, 0.57, 0.13)
marker.SetOrientationMarker(axes)
marker.SetViewport(0.85, 0.8, 1.1, 1.1)

marker.SetInteractor(iren)
marker.SetEnabled(1)
marker.InteractiveOff()


# picker

def myPickFun(object,event):
    global picker
    print 'plouf! ', picker.GetPickPosition()

picker = vtk.vtkCellPicker()
picker.AddObserver("EndPickEvent", myPickFun)
iren.SetPicker(picker)
    
print "\nuse <p> to pick an actor..."

iren.Initialize()
renWin.Render()
iren.Start()
    
print cam1
  
    