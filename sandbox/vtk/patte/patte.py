#! /usr/bin/env python
# -*- coding: latin-1; -*-

import vtk

def main():
    cutmesh('patte.stl', ymin=30, ymax=45, step=0.1)


def cutmesh(filename, ymin, ymax, step):
    
    # 1. setup pipeline
    # -----------------
    # STL mesh
    stlmesh = vtk.vtkSTLReader()
    stlmesh.SetFileName(filename)
    stlmesh.Update()

    # clip
    plane = vtk.vtkPlane()
    plane.SetOrigin(0, 30, 0)
    plane.SetNormal(0, 1, 0)

    cutter = vtk.vtkClipPolyData()
    cutter.SetClipFunction(plane)
    cutter.SetInputConnection(stlmesh.GetOutputPort())
    cutter.Update()

    massProps = vtk.vtkMassProperties()
    massProps.SetInputConnection(cutter.GetOutputPort())
    massProps.Update()

    # 2. move the plane and calculate the area 
    # ----------------------------------------
    def drange(start, stop, step):
        r = start
        while r < stop:
            yield r
            r += step

    curve = []
    for planey in drange(ymin, ymax, step):
        plane.SetOrigin(0, planey, 0)
        cutter.Update()
        srf = 0.0
        cut = cutter.GetOutput()
        if cut.GetNumberOfPoints():
            massProps.Update()
            srf = massProps.GetSurfaceArea()
        curve.append( (planey, srf) )

    # store the result in a matlab file
    outname = 'area.txt'
    with open(outname,'w') as outfile:
        for c in curve:
            outfile.write('%f %f\n' % c)
    print 'curve written in %s' % outname    


    # 3. display mesh + cut at y=ymin
    # -------------------------------

    plane.SetOrigin(0, ymin, 0)

    # STL mesh
    mapper = vtk.vtkPolyDataMapper()
    mapper.SetInputConnection(stlmesh.GetOutputPort())

    actor = vtk.vtkActor()
    actor.SetMapper(mapper)
    actor.GetProperty().SetColor(0.5, 0.5, 1.0)
    actor.GetProperty().SetOpacity(0.1)

    # STL grid
    mapper2 = vtk.vtkPolyDataMapper()
    mapper2.SetResolveCoincidentTopologyToPolygonOffset()
    mapper2.SetInputConnection(stlmesh.GetOutputPort())

    actor2 = vtk.vtkActor()
    actor2.SetMapper(mapper2)
    actor2.GetProperty().SetColor(0.5, 0.5, 1.0)
    actor2.GetProperty().SetAmbient(1.0)
    actor2.GetProperty().SetDiffuse(0.0)
    actor2.GetProperty().SetSpecular(0.0)
    actor2.GetProperty().SetRepresentationToWireframe()

    # clip
    clipMapper = vtk.vtkPolyDataMapper()
    clipMapper.SetInputConnection(cutter.GetOutputPort())

    clipActor = vtk.vtkActor()
    clipActor.GetProperty().SetColor(1.0, 1.0, 0.0)
    clipActor.SetMapper(clipMapper)

    # clip grid
    clipMapper2 = vtk.vtkPolyDataMapper()
    clipMapper2.SetResolveCoincidentTopologyToPolygonOffset()
    clipMapper2.SetInputConnection(cutter.GetOutputPort())

    clipActor2 = vtk.vtkActor()
    clipActor2.GetProperty().SetColor(0.0, 0.0, 0.0)
    clipActor2.GetProperty().SetAmbient(1.0)
    clipActor2.GetProperty().SetDiffuse(0.0)
    clipActor2.GetProperty().SetSpecular(0.0)
    clipActor2.GetProperty().SetRepresentationToWireframe()
    clipActor2.SetMapper(clipMapper2)

    # display mesh
    view = BasicView()

    cubeAxesActor = vtk.vtkCubeAxesActor()
    cubeAxesActor.SetBounds(stlmesh.GetOutput().GetBounds())
    cubeAxesActor.SetCamera(view.ren.GetActiveCamera())

    view.add( [actor, actor2, cubeAxesActor, clipActor, clipActor2])
    view.interact()


class BasicView(object):
    "defines a basic renderer / window / interactor"

    def __init__(self):
        ren = vtk.vtkRenderer()
        ren.SetBackground(0.1, 0.2, 0.4)

        win = vtk.vtkRenderWindow()
        win.SetSize(800, 800)
        win.AddRenderer(ren)

        intor = vtk.vtkRenderWindowInteractor()
        intor.SetRenderWindow(win)

        self.axes = ParaviewAxes(intor)
        self.ren = ren
        self.win = win
        self.intor = intor

    def add(self, actors):
        for actor in actors:
            self.ren.AddActor(actor) 
    
    def interact(self):
        # setup camera viewpoint
        #self.ren.GetActiveCamera().Azimuth(90)
        #self.ren.GetActiveCamera().Elevation(30)       
        self.ren.ResetCamera()
        self.win.Render()
        self.intor.Start() 


class ParaviewAxes(object):
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
        # keep track of both main objects
        self.axes = axes
        self.marker = marker


if __name__=='__main__':
    main()
