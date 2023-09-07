#!/usr/bin/env python3
# -*- coding: utf8 -*-

# fix VTK 9.1.0 debug python dll (.pyd) names after installation
#   (e.g. vtkFiltersSources.cp310-win_amd64_d.pyd => vtkFiltersSources_d.pyd)

import os
vtkdir = r'C:\local\VTK-9.1.0'

print("VTK dir =", vtkdir)

pyddir = os.path.join(vtkdir, "bin", "Lib", "site-packages", "vtkmodules")
if os.path.isdir(pyddir):
    nfiles = 0
    for f in os.listdir(pyddir):
        if f.endswith('_d.pyd'):
            if len(f.split('.')) == 3:
                newf = f.split('.')[0]+'_d.pyd'
                print(f, '=>', newf)
                os.rename(os.path.join(pyddir, f), os.path.join(pyddir, newf))
                nfiles += 1
    print(nfiles, "files renamed.")
else:
    print("Error:", pyddir, "not found!")

# powergreppe "VTKPython-targets-debug.cmake"

targetfile = os.path.join(vtkdir, 'lib','cmake','vtk-9.1','VTKPython-targets-debug.cmake')

if os.path.isfile(targetfile):
    print("powergrepping", targetfile)

    file = open(targetfile, mode='r')
    alllines = file.readlines()
    file.close()

    newlines = []
    for l in alllines:
        newl = l.replace(".cp310-win_amd64_d.pyd","_d.pyd")
        newlines.append(newl)
    
    file = open(targetfile, mode='w')
    for l in newlines:
        file.write(l)
    file.close()
else:
    print("Error:", targetfile, "not found!")

