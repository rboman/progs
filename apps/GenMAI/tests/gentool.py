#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
#   Copyright 2003-2019 Romain Boman
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

from __future__ import print_function
import sys
from builtins import range
from genmai import *

matrix = Tool()
builder = ToolBuilder(matrix)

if 0:
    builder.radius = 209.
    builder.initialAngle = 0.5
    builder.asperityLength = 0.05
    builder.asperityAngle = 21.8014
    builder.smoothnessAngle = 0.01
    builder.asperityInterval = 0.1
    builder.numberOfAsperities = 250
    builder.centre.x = -1.0
    builder.centre.y = 209.01

print(builder)

builder.genere()

print(matrix)


# vtk output
if not '--nogui' in sys.argv:
    import vtk
    ugrid = vtk.vtkUnstructuredGrid()
    points = vtk.vtkPoints()
    ugrid.SetPoints(points)

    print("converting points to vtk")
    for i in range(matrix.firstp, matrix.points.size()):
        points.InsertPoint(
            i-matrix.firstp, matrix.points[i].x, matrix.points[i].y, 0.0)

    print("converting curves to vtk")
    for i in range(matrix.firstc, matrix.curves.size()-1):  # ! shift!!
        nbp = len(matrix.curves[i].pts)
        if nbp == 2:
            cell = vtk.vtkLine()
            cell.GetPointIds().SetId(
                0, matrix.curves[i].pts[0] - matrix.firstp-1)
            cell.GetPointIds().SetId(
                1, matrix.curves[i].pts[1] - matrix.firstp-1)
        elif nbp == 3:
            cell = vtk.vtkQuadraticEdge()
            cell.GetPointIds().SetId(
                0, matrix.curves[i].pts[0] - matrix.firstp-1)
            cell.GetPointIds().SetId(
                1, matrix.curves[i].pts[2] - matrix.firstp-1)
            cell.GetPointIds().SetId(
                2, matrix.curves[i].pts[1] - matrix.firstp-1)
        else:
            raise Exception("curve with %d points" % nbp)

        ugrid.InsertNextCell(cell.GetCellType(), cell.GetPointIds())

    # save the grid
    writer = vtk.vtkXMLUnstructuredGridWriter()
    #compressor = vtk.vtkZLibDataCompressor()
    # writer.SetCompressor(compressor)
    writer.SetCompressorTypeToNone()
    writer.SetDataModeToAscii()
    writer.SetInputData(ugrid)
    writer.SetFileName('tool.vtu')
    writer.Write()

    # display (DEBUG)

    mapper = vtk.vtkDataSetMapper()
    mapper.SetInputData(ugrid)
    actor = vtk.vtkActor()
    actor.SetMapper(mapper)

    ren = vtk.vtkRenderer()

    renWin = vtk.vtkRenderWindow()
    renWin.SetSize(640, 480)
    renWin.AddRenderer(ren)
    iren = vtk.vtkRenderWindowInteractor()
    iren.SetRenderWindow(renWin)

    ren.AddActor(actor)
    ren.ResetCamera()

    iren.Initialize()
    renWin.Render()
    iren.Start()
