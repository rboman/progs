#! /usr/bin/env python3
#
#   Copyright 2006-2017 Romain Boman
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

# tests vtkTools
# RoBo - juin 2006

import vtk
import os
from vtkTools import *

datadir="../data/"

# test 1
print("\nTEST #1: readImageScalarRange")
readImageScalarRange(datadir+"lara/seg.img", extent=(0,255,0,255,0,59), coding='ushort')

# test 2
print("\nTEST #2: printOneLineOfData")
printOneLineOfData(datadir+"lara/seg.img", slice=30, line=128, extent=(0,255,0,255,0,59), coding='ushort')

# test 3
print("\nTEST #3: loadRawImage/extractOneSlice/displayOneSlice")
image = loadRawImage(datadir+"lara/seg.img",(0, 255, 0, 255, 0, 59),(0.98,0.98,1.56),'ushort','little')
image2 = extractOneSlice(image, slice=30)
print("close VTK window to continue...")
displayOneSlice(image2, slice=30, window=1, level=2)

# test 4
print("\nTEST #4: convert16to8bits")
convert16to8bits(datadir+"lara/seg.img", "seg2.img", extent=(0,255,0,255,0,59))
image = loadRawImage("seg2.img",(0, 255, 0, 255, 0, 59),(0.98,0.98,1.56),'uchar','little')
print("close VTK window to continue...")
displayOneSlice(image, slice=30, window=1, level=2)
#os.remove("seg2.img") # marche pas (permission denied)

# test 5
print("\nTEST #5: createContourActor/createOutlineActor/display3D")
image = loadRawImage(datadir+"lara/seg.img", (0, 255, 0, 255, 0, 59),(0.9375,0.9375,2.5),'ushort','little')
skin    = createContourActor(image)
outline = createOutlineActor(image)
print("close VTK window to continue...")
display3D((skin, outline))

# test 6
print("\nTEST #6: loadGenesisImage")
image = loadGenesisImage(datadir+"lara/006", (1,60))
print("close VTK window to continue...")
displayOneSlice(image, slice=30, window=255, level=127)

# test 7
print("\nTEST #7: loadRawImage (directory)")
image = loadRawImage(datadir+"lara/006",(0, 255, 0, 255, 1, 60),(0.98,0.98,1.56),'ushort','big')
print("close VTK window to continue...")
displayOneSlice(image, slice=30, window=255, level=127)

# test 8
print("\nTEST #8: off2vtk/savePolyData")
polydata = off2vtk(datadir+"tests/ellipse.off")
savePolyData("ellipse.vtk", polydata)
poly    = createPolyDataActor(polydata)
outline = createOutlineActor(polydata)
print("close VTK window to continue...")
display3D((poly, outline))

# test 9
print("\nTEST #9: loadPolyData")
polydata = loadPolyData(datadir+"tests/brain.vtk")
poly    = createPolyDataActor(polydata)
outline = createOutlineActor(polydata)
print("close VTK window to continue...")
display3D((poly, outline))

# test 10
print("\nTEST #10: createEuclide/createNegative/addImages (brain)")
image = loadRawImage(datadir+"lara/seg.img",(0, 255, 0, 255, 0, 59),(0.98,0.98,1.56),'ushort','little')
image2 = extractOneSlice(image, slice=13)
euclide1 = createEuclide(image2)
negative = createNegative(image2)
euclide2 = createEuclide(negative)
final = addImages(euclide1, euclide2)
polydata = createWarpPolyData(final, scale=1.0/19480*100)
actor = createPolyDataActor(polydata, showScalar=True, range=(0,2))
outline = createOutlineActor(polydata)
print("close VTK window to continue...")
display3D((actor, outline))

# test 11
print("\nTEST #11: createEuclide/createNegative/addImages (ellipse)")
image=createEllipsoid()
image2 = extractOneSlice(image, slice=80)
euclide1 = createEuclide(image2)
negative = createNegative(image2)
euclide2 = createEuclide(negative)
final = addImages(euclide1, euclide2)
polydata = createWarpPolyData(final, scale=1.0/19480*100)
actor = createPolyDataActor(polydata, showScalar=True, range=(0,2))
outline = createOutlineActor(polydata)
print("close VTK window to continue...")
display3D((actor, outline))

# test 12
print("\nTEST #12: createEllipsoid/saveRawImage/saveVtkImage")
image=createEllipsoid()
saveRawImage("ellipse.raw", image)
saveVtkImage("ellipse.vtk", image)

# test 13
print("\nTEST #12: loadVtkImage")
image = loadVtkImage(datadir+"tests/ellipse.vtk")
print("close VTK window to continue...")
displayOneSlice(image, slice=80, window=255, level=127)




