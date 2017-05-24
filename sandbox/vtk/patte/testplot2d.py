#! /usr/bin/env python
# -*- coding: utf-8; -*-

# very basic 2D plot using vtkXYPlotActor

import vtk
import math



def drange(start, stop, step):
    r = start
    while r < stop:
        yield r
        r += step

def genvalues():
    # generate a list of tuples
    values = []
    for x in drange(0.0, 4*math.pi, 4*math.pi/100):
        values.append( (x, math.sin(x)) )
    return values

def plot(values):

    # convert data from python list of tuples => vtkFieldData
    xCoords = vtk.vtkFloatArray()
    yCoords = vtk.vtkFloatArray()
    xCoords.SetNumberOfTuples(len(values))
    yCoords.SetNumberOfTuples(len(values))

    for i,v in enumerate(values):
        xCoords.SetTuple1(i, v[0])
        yCoords.SetTuple1(i, v[1])

    curve = vtk.vtkFieldData()
    curve.AddArray(xCoords)
    curve.AddArray(yCoords)

    # create vtkDataObject
    plot = vtk.vtkDataObject()
    plot.SetFieldData(curve)

    # build a vtkXYPlotActor
    xyplot = vtk.vtkXYPlotActor()
    xyplot.AddDataObjectInput(plot)
    #xyplot.SetDataObjectPlotModeToRows()
    xyplot.SetDataObjectXComponent(0, 0)
    xyplot.SetDataObjectYComponent(0, 1)
    xyplot.GetPositionCoordinate().SetValue(0, 0.0, 0)
    xyplot.GetPosition2Coordinate().SetValue(1, 1, 0)
    xyplot.PlotPointsOn()
    xyplot.PlotLinesOn()
    xyplot.GetProperty().SetPointSize(5)
    #xyplot.SetXRange(0, 100)
    #xyplot.SetYRange(0, 20)
    xyplot.SetPlotColor(0, 1, 1, 0)

    # setup renderer / window / interactor 
    ren = vtk.vtkRenderer()
    ren.SetBackground(0.1, 0.2, 0.4)
    ren.AddActor2D(xyplot)

    renWin = vtk.vtkRenderWindow()
    renWin.SetSize(1000, 800)
    renWin.AddRenderer(ren)

    iren = vtk.vtkRenderWindowInteractor()
    iren.SetRenderWindow(renWin)

    renWin.Render()
    iren.Start()

if __name__=="__main__":
    values = genvalues()
    plot(values)
