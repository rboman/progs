#! /usr/bin/env python3
#
# Affiche un fichier VRML 2 + capture
# RoBo - mai 2006
#

from __future__ import print_function
import vtk
import sys

#filename = "brain_bis_Rempli.wrl"
if len(sys.argv)!=2:
    print('usage: ShowVRML.py file.wrl')
    sys.exit()
filename = sys.argv[1]

ren = vtk.vtkRenderer()
renWin = vtk.vtkRenderWindow()
renWin.AddRenderer(ren)

importer = vtk.vtkVRMLImporter()
importer.SetRenderWindow(renWin)
importer.SetFileName(filename)
importer.Read()

iren = vtk.vtkRenderWindowInteractor()
iren.SetRenderWindow(renWin)

importer.GetRenderer().SetBackground(0.1, 0.2, 0.4)
importer.GetRenderWindow().SetSize(600, 600)

renWin.Render()

# capture png (x2)
renderLarge = vtk.vtkRenderLargeImage()
renderLarge.SetInput(ren)
renderLarge.SetMagnification(2)
writer = vtk.vtkPNGWriter()
writer.SetInputConnection(renderLarge.GetOutputPort())
writer.SetFileName(sys.argv[1]+".png")
writer.Write()

#print ren

iren.Start()
