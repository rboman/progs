# -*- coding: utf-8 -*-
#
# inspired from:
#   https://kitware.github.io/vtk-examples/site/Python/PolyData/ExtractSelection/
#   https://vtk.org/Wiki/VTK/Examples/Cxx/Picking/CellPicking 

import vtk
colors = vtk.vtkNamedColors()
# print(colors.GetColorNames())
# see also https://vtk.org/Wiki/VTK/Examples/Python/Visualization/VTKNamedColorPatches_html

from mytools import *
from myinteractor import *
from myview import *


def main():
    # ugrid = loadUGrid('Dolicorhynchops_coarse.vtu')
    ugrid = loadUGrid('grid.vtu')
    # saveUGrid(ugrid, 'grid.vtu')


    print('There are %s input points' % ugrid.GetNumberOfPoints())
    print('There are %s input cells' % ugrid.GetNumberOfCells())

    # mesh
    meshMapper = vtk.vtkDataSetMapper() 
    meshMapper.SetInputData(ugrid)
    meshActor = vtk.vtkActor()
    meshActor.GetProperty().SetColor(colors.GetColor3d('white'))
    meshActor.GetProperty().EdgeVisibilityOn()
    meshActor.SetMapper(meshMapper)
    
    # display mesh
    view = BasicView()
    view.add( [meshActor] ) 

    # cubeAxesActor = vtk.vtkCubeAxesActor()
    # cubeAxesActor.SetBounds(ugrid.GetBounds())
    # cubeAxesActor.SetCamera(view.ren.GetActiveCamera())


    # -- example of callback - rien a voir avec la selection
    # print the camaera position for each render
    if 0:
        def start_callback(caller, ev):
            # print('StartEvent')
            position = view.ren.GetActiveCamera().GetPosition()
            print('StartEvent ({:5.2f}, {:5.2f}, {:5.2f})'.format(*position))
        view.ren.AddObserver('StartEvent', start_callback)

    view.interact()



if __name__=="__main__":
    main()

