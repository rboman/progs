#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import os
import sys

if os.path.basename(sys.executable)=="python_d.exe":
    config='Debug'
elif os.path.basename(sys.executable)=="python.exe":
    config='Release'
else:
    config=''

root = os.path.dirname(__file__)
pyd_folder = os.path.join(root, 'build', 'bin', config)
dll_folder = pyd_folder

print(f'adding {pyd_folder} to PYTHONPATH')
sys.path.append(pyd_folder)

print(f'adding {dll_folder} to add_dll_directory')
try:
    os.add_dll_directory(dll_folder)
except:
    pass

# "add_dll_directory" de vtk nécessaire avant l'import...
import vtk
from vtkbool.vtkBool import vtkBooleanOperationPolyDataFilter71

cube = vtk.vtkSphereSource()
cube.SetCenter(0, 0, 0)
cube.SetThetaResolution(20)
cube.SetPhiResolution(20)

sphere = vtk.vtkSphereSource()
sphere.SetCenter(.5, .5, .5)
sphere.SetThetaResolution(20)
sphere.SetPhiResolution(20)

boolean = vtkBooleanOperationPolyDataFilter71()
boolean.SetInputConnection(0, cube.GetOutputPort())
boolean.SetInputConnection(1, sphere.GetOutputPort())
boolean.SetOperationToDifference()

# save to disk...
# writer = vtk.vtkPolyDataWriter()
# writer.SetInputConnection(boolean.GetOutputPort())
# writer.SetFileName('result.vtk')
# writer.Update()

# basic display of the result...
mapper = vtk.vtkPolyDataMapper()
mapper.SetInputConnection(boolean.GetOutputPort())

actor = vtk.vtkActor()
actor.SetMapper(mapper)

ren = vtk.vtkRenderer()
renWin = vtk.vtkRenderWindow()
renWin.AddRenderer(ren)
iren = vtk.vtkRenderWindowInteractor()
iren.SetRenderWindow(renWin)
ren.AddActor(actor)
renWin.SetSize(600, 600)
renWin.SetWindowName('filter example')
iren.Initialize()
iren.Start()
