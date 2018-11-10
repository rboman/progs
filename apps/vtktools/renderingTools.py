#! /usr/bin/env python
# -*- coding: latin-1; -*-
# $Id: renderingTools.py 1709 2013-03-11 14:49:09Z papeleux $
# Vinciane d'Otreppe

# Vtk visualisation functions

import vtk
import generalTools
    
def displaySlice(image, slice=30, window=255, level=127):
    viewer = vtk.vtkImageViewer2() # permet de zoomer dans l'image
    viewer.SetInput(image)
    viewer.SetSlice(slice)
    viewer.SetColorWindow(window)
    viewer.SetColorLevel(level)
    viewer.Render()

    interact = vtk.vtkRenderWindowInteractor()
    viewer.SetupInteractor(interact)
    interact.Initialize()
    interact.Start()    
              
def displayPolyAnd3Planes(poly, image, title = 'Polydata and 3 Planes'):
    """
    display a poly and 3 planes
    """
    
    mapper = vtk.vtkPolyDataMapper()
    mapper.SetInput(poly)

    actor = vtk.vtkActor()
    actor.SetMapper(mapper)
    actor.GetProperty().SetRepresentationToWireframe()
    actor.GetProperty().SetColor(0,0,0)
    
    ren= vtk.vtkRenderer()
    ren.AddActor(actor)
    ren.SetBackground(1,1,1)

    renwin = vtk.vtkRenderWindow()
    renwin.AddRenderer(ren)
    renwin.SetSize( 700, 700 )
    renwin.SetWindowName(title)
    
    iren = vtk.vtkRenderWindowInteractor()
    iren.SetRenderWindow(renwin)
    style = vtk.vtkInteractorStyleTrackballCamera()
    iren.SetInteractorStyle(style)
    wX,wY,wZ = create3Planes(image)

    wX.SetInteractor(iren); wX.On()
    wY.SetInteractor(iren); wY.On()
    wZ.SetInteractor(iren); wZ.On()

    outline = createOutlineActor(image)
    ren.AddActor(outline)
    
    axes = vtk.vtkCubeAxesActor2D()
    axes.SetCamera(ren.GetActiveCamera())
    axes.SetViewProp(outline)
    ren.AddActor(axes)

    ren.ResetCamera()
    cam1 = ren.GetActiveCamera()
    cam1.Elevation(-90)
    cam1.SetViewUp(0, 0, 1)
    cam1.Azimuth(45)
    ren.ResetCameraClippingRange()
        
    iren.Initialize()
    renwin.Render()
    iren.Start()
    
def view3Planes(image, title = 'Image and 3 Planes'):
    """
    display and image and 3 planes
    """
    
    xMin, xMax, yMin, yMax, zMin, zMax = image.GetWholeExtent()
    
    ren= vtk.vtkRenderer()
    ren.SetBackground(0.2,0.3,0.6)

    renwin = vtk.vtkRenderWindow()
    renwin.AddRenderer(ren)
    renwin.SetSize( 700, 700 )
    renwin.SetWindowName(title)
    
    iren = vtk.vtkRenderWindowInteractor()
    iren.SetRenderWindow(renwin)
    style = vtk.vtkInteractorStyleTrackballCamera()
    iren.SetInteractorStyle(style)
    wX,wY,wZ = create3Planes(image)

    wX.SetInteractor(iren); wX.On()
    wY.SetInteractor(iren); wY.On()
    wZ.SetInteractor(iren); wZ.On()

    outline = createOutlineActor(image)
    ren.AddActor(outline)
    
    axes = vtk.vtkCubeAxesActor2D()
    axes.SetCamera(ren.GetActiveCamera())
    axes.SetViewProp(outline)
    ren.AddActor(axes)

    ren.ResetCamera()
    cam1 = ren.GetActiveCamera()
    cam1.Elevation(-70)
    cam1.SetViewUp(0, 0, 1)
    cam1.Azimuth(30)
    ren.ResetCameraClippingRange()
        
    iren.Initialize()
    renwin.Render()
    iren.Start() 
   
def display2UG(UG1, UG2, title = 'Two Unstructured Grids'):
    """
    display 2 data sets (unstructured grid)
    """
    
    mapper1 = vtk.vtkDataSetMapper()
    mapper1.SetInput(UG1)

    actor1 = vtk.vtkActor()
    actor1.SetMapper(mapper1)

    mapper2 = vtk.vtkDataSetMapper()
    mapper2.SetInput(UG2)

    actor2 = vtk.vtkActor()
    actor2.GetProperty().SetColor(1,0,0)
    actor2.SetMapper(mapper2)
    
    display3D( (actor1, actor2), title)
   
def createLookupTable(nbcol=16):
    lookup = vtk.vtkLookupTable()
    lookup.SetNumberOfColors(nbcol)
    lookup.SetHueRange(0.66667, 0.)
    
    lookup.Build()
    return lookup
    
def display3D(actors, title):  
    aRenderer = vtk.vtkRenderer()
    aRenderer.SetBackground(1.0, 1.0, 1.0) 
    for actor in actors:
        aRenderer.AddActor(actor)
        
    renWin = vtk.vtkRenderWindow()
    renWin.SetSize(700, 700)    
    renWin.AddRenderer(aRenderer)
    renWin.Render()
    renWin.SetWindowName(title)
        
    iren = vtk.vtkRenderWindowInteractor()
    iren.SetRenderWindow(renWin)

    iren.Initialize()
    iren.Start()

def create3Planes(image):
    xMin, xMax, yMin, yMax, zMin, zMax = image.GetWholeExtent()

    picker = vtk.vtkCellPicker()
    picker.SetTolerance(0.005)
    
    wX = vtk.vtkImagePlaneWidget()
    wX.DisplayTextOn()
    wX.SetInput(image)
    wX.SetPlaneOrientationToXAxes()
    wX.SetSliceIndex((xMax-xMin)/2)
    wX.SetKeyPressActivationValue("x")
    wX.SetResliceInterpolateToLinear()
    wX.SetPicker(picker)
    wX.GetTextProperty().SetColor(1,0,0)
    wX.GetTextProperty().SetLineOffset(70)  
    wX.GetTextProperty().BoldOn ()
    wX.GetTextProperty().SetFontFamilyToArial() 
    wX.GetTextProperty().SetFontSize(12) 
    wX.GetPlaneProperty().SetColor(1, 0, 0)

    wY = vtk.vtkImagePlaneWidget()
    wY.DisplayTextOn()
    wY.SetInput(image)
    wY.SetPlaneOrientationToYAxes()
    wY.SetSliceIndex((yMax-yMin)/2)
    wY.SetKeyPressActivationValue("y")
    wY.SetResliceInterpolateToLinear()
    wY.SetPicker(picker)
    wY.GetTextProperty().SetColor(1,0,0)
    wY.GetTextProperty().SetLineOffset(70)
    wY.GetTextProperty().BoldOn ()
    wY.GetTextProperty().SetFontFamilyToArial() 
    wY.GetTextProperty().SetFontSize(12) 
    wY.GetPlaneProperty().SetColor(1, 1, 0)  	
    wY.SetLookupTable(wX.GetLookupTable())

    wZ = vtk.vtkImagePlaneWidget()
    wZ.DisplayTextOn()
    wZ.SetInput(image)
    wZ.SetPlaneOrientationToZAxes()
    wZ.SetSliceIndex((zMax-zMin)/2)
    wZ.SetKeyPressActivationValue("z")
    wZ.SetResliceInterpolateToLinear()
    wZ.SetPicker(picker)
    wZ.GetTextProperty().SetColor(1,0,0)
    wZ.GetTextProperty().SetLineOffset(70) 
    wZ.GetTextProperty().BoldOn () 
    wZ.GetTextProperty().SetFontFamilyToArial() 
    wZ.GetTextProperty().SetFontSize(12) 
    wZ.GetPlaneProperty().SetColor(0, 0, 1)
    wZ.SetLookupTable(wX.GetLookupTable())
    
    return wX,wY,wZ 

def createOutlineActor(image):
    outlineData = vtk.vtkOutlineFilter()
    outlineData.SetInput(image)
    mapOutline = vtk.vtkPolyDataMapper()
    mapOutline.SetInputConnection(outlineData.GetOutputPort())
    outline = vtk.vtkActor()
    outline.SetMapper(mapOutline)
    outline.GetProperty().SetColor(0, 0, 0)
    return outline

def createGridActor(grid, showScalar=False, range=None, showEdges=False, colorMap='colors'):
    mapper = vtk.vtkDataSetMapper()
    mapper.SetInput(grid)
    actor = vtk.vtkActor()
    if showScalar:
        scalars = grid.GetPointData().GetScalars()
        if scalars==None: 
            scalars = grid.GetCellData().GetScalars()
        if scalars==None:
            scalars = grid.GetPointData().GetArray(0)
            grid.GetPointData().SetScalars(scalars)
        if scalars==None:
            scalars = grid.GetCellData().GetArray(0)
            grid.GetCellData().SetScalars(scalars)
        if range==None: range = [scalars.GetRange()[0],.8*scalars.GetRange()[1]]
        mapper.SetScalarRange(*range)
        if colorMap=='GrayScale':wl = vtk.vtkWindowLevelLookupTable()
        else:
            wl = vtk.vtkLookupTable()
            wl.SetScaleToLinear()
            wl.SetRange(*range)
            wl.SetHueRange(0.6667,0.0)#blue to red
            wl.Build()
        mapper.SetLookupTable(wl)
    else:
        mapper.ScalarVisibilityOff()
        actor.GetProperty().SetColor(.6, .6, .6)
    actor.SetMapper(mapper)
    if showEdges: actor.GetProperty().EdgeVisibilityOn()
    return actor

def createClipActor(grid, plane, showScalar=False, range=None, showEdges=False, colorMap='GrayScale'):
    clipMapper = vtk.vtkDataSetMapper()
    clipMapper.SetInput(grid)
    cutMapper = vtk.vtkDataSetMapper()
    clipActor = vtk.vtkActor()
    cutActor = vtk.vtkActor()
    if showScalar:
        scalars = grid.GetPointData().GetScalars()
        if scalars==None: 
            scalars = grid.GetCellData().GetScalars()
        if scalars==None:
            scalars = grid.GetPointData().GetArray(0)
            grid.GetPointData().SetScalars(scalars)
        if scalars==None:
            scalars = grid.GetCellData().GetArray(0)
            grid.GetCellData().SetScalars(scalars)
        if range==None: range = [scalars.GetRange()[0],.8*scalars.GetRange()[1]]
        cutMapper.SetScalarRange(*range)
        clipMapper.SetScalarRange(*range)
        if colorMap=='GrayScale':wl = vtk.vtkWindowLevelLookupTable()
        else:
            wl = vtk.vtkLookupTable()
            wl.SetScaleToLinear()
            wl.SetRange(*range)
            wl.SetHueRange(0.6667,0.0)#blue to red
            wl.Build()
        clipMapper.SetLookupTable(wl)
        cutMapper.SetLookupTable(wl)
    else:
        clipMapper.ScalarVisibilityOff()
        cutMapper.ScalarVisibilityOff()
        clipActor.GetProperty().SetColor(.6, .6, .6)
        cutActor.GetProperty().SetColor(.6, .6, .6)
    clipMapper.AddClippingPlane(plane)
    clipActor.SetMapper(clipMapper)

    cutter = vtk.vtkCutter()
    cutter.SetCutFunction(plane)
    cutter.SetInput(grid)
    cutter.Update()
    cutMapper.SetInputConnection(cutter.GetOutputPort())
    cutActor.SetMapper(cutMapper)
    if showEdges: 
        cutActor.GetProperty().EdgeVisibilityOn()
        clipActor.GetProperty().EdgeVisibilityOn()
    return [cutActor,clipActor]
    
    
def createPlaneWidget(ugrid):
    planeWidget = vtk.vtkImplicitPlaneWidget()
    planeWidget.SetPlaceFactor(1.25)
    planeWidget.SetInput(ugrid)
    planeWidget.PlaceWidget(ugrid.GetBounds())
    planeWidget.NormalToXAxisOn()
    planeWidget.DrawPlaneOff()
    planeWidget.TubingOn()
    planeWidget.OutlineTranslationOff()
    planeWidget.ScaleEnabledOff()
    planeWidget.GetNormalProperty().SetColor(.6, .1, 1)
    planeWidget.GetSelectedNormalProperty().SetColor(.4, .4, 4)
    planeWidget.GetEdgesProperty().SetLineWidth(.6)
    planeWidget.SetHandleSize(.01)
    return planeWidget


def createBoxWidget(image, iren):
    boxWidget = vtk.vtkBoxWidget()
    boxWidget.SetInteractor(iren)
    boxWidget.SetPlaceFactor(1.0)
    boxWidget.SetInput(image)
    boxWidget.PlaceWidget()
    boxWidget.InsideOutOn()
    outlineProperty = boxWidget.GetOutlineProperty()
    outlineProperty.SetRepresentationToWireframe()
    outlineProperty.SetAmbient(1.0)
    outlineProperty.SetAmbientColor(0, 0, 0)
    outlineProperty.SetLineWidth(2)
    selectedOutlineProperty = boxWidget.GetSelectedOutlineProperty()
    selectedOutlineProperty.SetRepresentationToWireframe()
    selectedOutlineProperty.SetAmbient(1.0)
    selectedOutlineProperty.SetAmbientColor(1, 0, 0)
    selectedOutlineProperty.SetLineWidth(3)
    return boxWidget
     
def displayVolume(image,title='volume'):
    '''
    display a volume image - image must be a typical irm image with gray values comprised between 0 and 255
    '''
    image = generalTools.castImage(image,'uchar')
    volume, mapper = createVolume(image)
    
    ren1 = vtk.vtkRenderer()
    ren1.AddVolume(volume)
    
    renWin = vtk.vtkRenderWindow()
    renWin.SetSize(700, 700)    
    renWin.AddRenderer(ren1)
    renWin.Render()
    renWin.SetWindowName(title)
    
    iren = vtk.vtkRenderWindowInteractor()
    iren.SetRenderWindow(renWin)

    iren.Initialize()
    iren.Start()

def displayPoints(vtkpoints):
    polydata = vtk.vtkPolyData()
    polydata.SetPoints(vtkpoints);
    mask = vtk.vtkMaskPoints()
    mask.SetInput(polydata)
    mask.GenerateVerticesOn()
    poly = mask.GetOutput()
    import gui2
    gui = gui2.MeshViewer()
    gui.add(gui2.SurfMesh(poly,name,"black"))
    gui.start()    

def createVolume(image):
    '''
    create volume of image
    '''
    sMin,sMax = image.GetScalarRange()
    def rescaleToGV(i):
        return sMin+(sMax-sMin)/255*i
    # Create transfer mapping scalar value to opacity
    opacityTransferFunction = vtk.vtkPiecewiseFunction()
    opacityTransferFunction.AddPoint(0, 0.0)
    opacityTransferFunction.AddPoint(10.0, 0.0)
    opacityTransferFunction.AddPoint(40.0, 1.0)
    opacityTransferFunction.AddPoint(60, 1.0)
    opacityTransferFunction.AddPoint(200, 0.0)
    opacityTransferFunction.AddPoint(255, 0.0)
    # Create transfer mapping scalar value to color
    colorTransferFunction = vtk.vtkColorTransferFunction()
    colorTransferFunction.AddRGBPoint(0.0, 0.0, 0.0, 0.0)
    colorTransferFunction.AddRGBPoint(64.0, 1.0, 0.0, 0.0)
    colorTransferFunction.AddRGBPoint(128.0, 0.0, 0.0, 1.0)
    colorTransferFunction.AddRGBPoint(192.0, 0.0, 1.0, 0.0)
    colorTransferFunction.AddRGBPoint(255.0, 0.0, 0.2, 0.0)
    # The property describes how the data will look
    volumeProperty = vtk.vtkVolumeProperty()
    volumeProperty.SetColor(colorTransferFunction)
    volumeProperty.SetScalarOpacity(opacityTransferFunction)
    volumeProperty.ShadeOn()
    volumeProperty.SetInterpolationTypeToLinear()
    volumeProperty.SetDiffuse(0.7)
    volumeProperty.SetSpecular(0.5)
    volumeProperty.SetSpecularPower(70.0)
    # mapper
    volumeMapper=None
    if 1:
        compositeFunction = vtk.vtkVolumeRayCastCompositeFunction()
        volumeMapper = vtk.vtkVolumeRayCastMapper()
        volumeMapper.SetVolumeRayCastFunction(compositeFunction)
        volumeMapper.SetInput(image)
    else:
        volumeMapper = vtk.vtkVolumeTextureMapper2D()
        volumeMapper.SetInput(image)
    # volume
    volume = vtk.vtkVolume()
    volume.SetMapper(volumeMapper)
    volume.SetProperty(volumeProperty)
    return volume, volumeMapper
