# -*- coding: utf-8 -*-


import vtk
from myinteractor import *
from myinteractor2 import *
colors = vtk.vtkNamedColors()

class BasicView:
    "defines a basic renderer / window / interactor"

    def __init__(self):
        
        ren = vtk.vtkRenderer()
        ren.SetBackground(colors.GetColor3d('cobalt') )

        win = vtk.vtkRenderWindow()
        win.SetSize(800, 800)
        win.SetWindowName('Select faces')
        win.AddRenderer(ren)

        intor = vtk.vtkRenderWindowInteractor()
        intor.SetRenderWindow(win)

        # custom interactor style - sect one cell
        # sw = MyInteractorStyle()
        # sw.SetDefaultRenderer(ren)

        # rubber band
        istyle = vtk.vtkInteractorStyleRubberBandPick()
        istyle.SetDefaultRenderer(ren)
        areaPicker = vtk.vtkAreaPicker()
        pick_callback = MyPickAreaCallBack()
        areaPicker.AddObserver('EndPickEvent', pick_callback)
        intor.SetPicker(areaPicker)

        intor.SetInteractorStyle(istyle)
       
        self.axes = ParaviewAxes(intor)
        self.ren = ren
        self.win = win
        self.intor = intor

    def add(self, actors):
        for actor in actors:
            self.ren.AddActor(actor) 
    
    def interact(self):
        self.ren.ResetCamera() # camera en fct de l'objet (key = 'r')
        # self.win.Render()           # semble inutile
        # self.intor.Initialize()     # semble inutile (?)  
        self.intor.Start() 


class ParaviewAxes:
    "axes a la paraview"

    def __init__(self, intor):
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
        axes.GetYAxisCaptionActor2D().SetCaptionTextProperty(tprop)
        axes.GetZAxisCaptionActor2D().SetCaptionTextProperty(tprop)
        marker = vtk.vtkOrientationMarkerWidget()
        marker.SetOrientationMarker(axes)
        marker.SetViewport(0.85, 0.8, 1.1, 1.1)
        marker.SetInteractor(intor)
        marker.SetEnabled(1)
        marker.InteractiveOff()
        # keep track of both main objects (otherwise, the display hangs!)
        self.axes = axes
        self.marker = marker


