#! /usr/bin/env python
# $Id$
# crée les fichiers nécessaires au calcul sphere-elipse du TFE Yala
# RoBo - juin 2006

import vtk
from vtkTools import *

# sphere
sphere = createSphere(extent=(0,99,0,99,0,99), center=(50, 50, 50), radius=30, coding='ushort')
saveRawImage("sphere30.raw", sphere)
saveVtkImage("sphere30.vtk", sphere)

# maillage isosurf
isosurfExec="E:\Boman\Work\Projets\Cerveau\Recup_Yala\Software\IsoSurf\isosurf.v1_5d.exe"
isosurfExec="isosurf.exe"
#range=(1,65535)
range=(1,255)
inputFile="\"E:\Boman\Work\Projets\Cerveau\Recup_Yala\Mine\python\sphere.raw\""
#import win32api
#inputFile=win32api.GetShortPathName(inputFile)
extent=(100,100,100)
scale=(1,1,1)
resolution=2
coding='ushort'
codflag='s' # 'c'='uchar' 'u'='ushort'
cmd = "\"%s\" -t %d,%d -i %s -d %d,%d,%d -s %d,%d,%d -r %d -f %s" % (isosurfExec, 
      range[0], range[1], inputFile, extent[0],extent[1], extent[2], \
      scale[0], scale[1], scale[2], resolution, codflag);
      
print "exec:", cmd
import os
os.system(cmd)

polydata = off2vtk(name="surface.off")
savePolyData("sphere30_poly.vtk", polydata)
if 1:
	poly    = createPolyDataActor(polydata)
	outline = createOutlineActor(polydata)
	print "close VTK window to continue..."
	display3D((poly, outline))

#ellipse
ellipse = createEllipsoid(extent=(0,99,0,99,0,99), center=(50, 50, 50), radius=(40, 40, 40), coding='ushort')
saveRawImage("sphere40.raw", sphere)
saveVtkImage("sphere40.vtk", sphere)

# energy map ellipse
euclide1 = createEuclide(ellipse)
negative = createNegative(ellipse)
euclide2 = createEuclide(negative)
final = addImages(euclide1, euclide2)
print 'extent=',final.GetExtent()
print 'spacing=',final.GetSpacing()

saveVtkImage("sphere-30-40_emap.vtk", final)
