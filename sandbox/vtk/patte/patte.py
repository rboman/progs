#! /usr/bin/env python
# -*- coding: latin-1; -*-

import vtk

def main():
    view('patte.stl', ymin=30, ymax=45, step=0.1)


def view(filename, ymin, ymax, step):
    
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

    plane.SetOrigin(0, ymin, 0)

    # store result in matlab file
    outname = 'area.txt'
    with open(outname,'w') as outfile:
        for c in curve:
            outfile.write('%f %f\n' % c)
    print 'curve written in %s' % outname    


    # 3. display mesh + cut at y=ymin
    # -------------------------------

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

    # renderer / window / interactor
    ren = vtk.vtkRenderer()
    win = vtk.vtkRenderWindow()
    win.SetSize(800, 800)
    win.AddRenderer(ren)

    intor = vtk.vtkRenderWindowInteractor()
    intor.SetRenderWindow(win)

    ren.AddActor(actor)
    ren.AddActor(actor2)
    ren.AddActor(clipActor)
    ren.AddActor(clipActor2)
    ren.SetBackground(0.1, 0.2, 0.4)

    cubeAxesActor = vtk.vtkCubeAxesActor()
    cubeAxesActor.SetBounds(stlmesh.GetOutput().GetBounds())
    cubeAxesActor.SetCamera(ren.GetActiveCamera())

    ren.AddActor(cubeAxesActor)
    #ren.GetActiveCamera().Azimuth(30)
    #ren.GetActiveCamera().Elevation(30)
    ren.ResetCamera()

    # axes a la paraview -----
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
    tprop2 = vtk.vtkTextProperty() # inutile
    tprop2.ShallowCopy(tprop) # inutile
    axes.GetYAxisCaptionActor2D().SetCaptionTextProperty(tprop2)
    tprop3 = vtk.vtkTextProperty() # inutile
    tprop3.ShallowCopy(tprop) # inutile
    axes.GetZAxisCaptionActor2D().SetCaptionTextProperty(tprop3)

    marker = vtk.vtkOrientationMarkerWidget()
    #marker.SetOutlineColor(0.93, 0.57, 0.13)
    marker.SetOrientationMarker(axes)
    marker.SetViewport(0.85, 0.8, 1.1, 1.1)

    marker.SetInteractor(intor)
    marker.SetEnabled(1)
    marker.InteractiveOff()
    # axes a la paraview -----

    win.Render()
    intor.Start()




if __name__=='__main__':
    main()
