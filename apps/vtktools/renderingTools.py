#! /usr/bin/env python3
# -*- coding: utf-8; -*-

# Vtk visualisation functions

from past.utils import old_div
import vtk
import generalTools
import meshingTools


def displaySlice(image, slice=30, window=255, level=127):
    viewer = vtk.vtkImageViewer2()  # permet de zoomer dans l'image
    viewer.SetInput(image)
    viewer.SetSlice(slice)
    viewer.SetColorWindow(window)
    viewer.SetColorLevel(level)
    viewer.Render()

    interact = vtk.vtkRenderWindowInteractor()
    viewer.SetupInteractor(interact)
    interact.Initialize()
    interact.Start()


def displayPolyAnd3Planes(poly, image, title='Polydata and 3 Planes'):
    """
    display a poly and 3 planes
    """

    mapper = vtk.vtkPolyDataMapper()
    mapper.SetInput(poly)

    actor = vtk.vtkActor()
    actor.SetMapper(mapper)
    actor.GetProperty().SetRepresentationToWireframe()
    actor.GetProperty().SetColor(0, 0, 0)  # RED

    ren = vtk.vtkRenderer()
    ren.AddActor(actor)
    ren.SetBackground(1, 1, 1)

    renwin = vtk.vtkRenderWindow()
    renwin.AddRenderer(ren)
    renwin.SetSize(700, 700)
    renwin.SetWindowName(title)

    iren = vtk.vtkRenderWindowInteractor()
    iren.SetRenderWindow(renwin)
    style = vtk.vtkInteractorStyleTrackballCamera()
    iren.SetInteractorStyle(style)
    wX, wY, wZ = create3Planes(image)

    wX.SetInteractor(iren)
    wX.On()
    wY.SetInteractor(iren)
    wY.On()
    wZ.SetInteractor(iren)
    wZ.On()

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


def displayPolyAndDataLabels(poly, title='Polydata'):
    """
    display a poly and 3 planes
    """
    mapper = vtk.vtkPolyDataMapper()
    mapper.SetInput(poly)

    actor1 = vtk.vtkActor()
    actor1.SetMapper(mapper)
    actor1.GetProperty().SetColor(1, 0, 0)  # RED
    actor1.GetProperty().SetOpacity(1.0)

    mapper2 = vtk.vtkPolyDataMapper()
    mapper2.SetResolveCoincidentTopologyToPolygonOffset()
    mapper2.ScalarVisibilityOff()
    mapper2.SetInput(poly)

    actor2 = vtk.vtkActor()
    actor2.SetMapper(mapper2)
    actor2.GetProperty().SetRepresentationToWireframe()
    actor2.GetProperty().SetColor(1, 1, 1)
    actor2.GetProperty().SetAmbient(1.0)
    actor2.GetProperty().SetDiffuse(0.0)
    actor2.GetProperty().SetSpecular(0.0)

    ids = vtk.vtkIdFilter()
    ids.SetInput(poly)
    ids.PointIdsOn()
    ids.CellIdsOn()
    ids.FieldDataOn()

    ren = vtk.vtkRenderer()
    ren.AddActor(actor1)
    ren.AddActor(actor2)
    ren.SetBackground(1, 1, 1)

    # Create labels for points
    visPts = vtk.vtkSelectVisiblePoints()
    visPts.SetInputConnection(ids.GetOutputPort())
    visPts.SetRenderer(ren)

    # Create the mapper to display the point ids.  Specify the format to
    # use for the labels.  Also create the associated actor.
    ldm = vtk.vtkLabeledDataMapper()
    ldm.GetLabelTextProperty().ShadowOff()
    ldm.GetLabelTextProperty().SetColor(0, 0, 0)
    # ldm.SetLabelFormat("%g")
    ldm.SetInputConnection(visPts.GetOutputPort())
    ldm.SetLabelModeToLabelFieldData()
    pointLabels = vtk.vtkActor2D()
    pointLabels.SetMapper(ldm)
    ren.AddActor2D(pointLabels)

    renwin = vtk.vtkRenderWindow()
    renwin.AddRenderer(ren)
    renwin.SetSize(700, 700)
    renwin.SetWindowName(title)

    iren = vtk.vtkRenderWindowInteractor()
    iren.SetRenderWindow(renwin)
    style = vtk.vtkInteractorStyleTrackballCamera()
    iren.SetInteractorStyle(style)

    ren.ResetCamera()
    cam1 = ren.GetActiveCamera()
    cam1.Elevation(-90)
    cam1.SetViewUp(0, 0, 1)
    cam1.Azimuth(45)
    ren.ResetCameraClippingRange()

    iren.Initialize()
    renwin.Render()
    iren.Start()


def view3Planes(image, title='Image and 3 Planes'):
    """
    display and image and 3 planes
    """

    xMin, xMax, yMin, yMax, zMin, zMax = image.GetWholeExtent()

    ren = vtk.vtkRenderer()
    ren.SetBackground(0.2, 0.3, 0.6)

    renwin = vtk.vtkRenderWindow()
    renwin.AddRenderer(ren)
    renwin.SetSize(700, 700)
    renwin.SetWindowName(title)

    iren = vtk.vtkRenderWindowInteractor()
    iren.SetRenderWindow(renwin)
    style = vtk.vtkInteractorStyleTrackballCamera()
    iren.SetInteractorStyle(style)
    wX, wY, wZ = create3Planes(image)

    wX.SetInteractor(iren)
    wX.On()
    wY.SetInteractor(iren)
    wY.On()
    wZ.SetInteractor(iren)
    wZ.On()

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


def getDistancesBetweenTwoPolysScalarsLara(poly1, poly2, axis='a', title='Distance between two polydatas'):
    """
    display poly1 and distance values between points of poly2 and poly1
    axis = {'a','x','y','z'}    'a' = euclidean distance between two points; 'x' = x axis, 'y'= y axis, 'z' = z axis
    """
    import math
    darray = vtk.vtkDoubleArray()
    darray.SetNumberOfComponents(1)
    nbpt = poly1.GetPoints().GetNumberOfPoints()
    darray.SetNumberOfValues(nbpt)
    darray.SetName('distance between polys')
    if nbpt != poly2.GetPoints().GetNumberOfPoints():
        print(' ERROR in renderingTools.displayDistancesBetweenTwoPolys() : poly1 and poly must have same number of nodes ')

    for ii in range(0, nbpt):
        pointIni = poly1.GetPoint(ii)
        pointFin = poly2.GetPoint(ii)
        dispX = (pointIni[0] - pointFin[0])
        dispY = (pointIni[1] - pointFin[1])
        dispZ = (pointIni[2] - pointFin[2])
        if axis == 'x':
            displ = dispX
        elif axis == 'y':
            displ = dispY
        elif axis == 'z':
            displ = dispZ
        elif axis == 'a':
            displ = math.sqrt(dispX*dispX+dispY*dispY+dispZ*dispZ)
        darray.InsertValue(ii, displ)

    return darray


def setDistancesBetweenTwoPolysScalars(refPoly, poly2):
    """
    display poly1 and distance values between points of poly2 and poly1
    axis = {'a','x','y','z'}    'a' = euclidean distance between two points; 'x' = x axis, 'y'= y axis, 'z' = z axis
    """
    import math

    # double array to store distances
    darray = vtk.vtkDoubleArray()
    darray.SetNumberOfComponents(1)
    nbpt = refPoly.GetPoints().GetNumberOfPoints()
    darray.SetNumberOfValues(nbpt)

    # cellLocator needed to locate nearest point on the reference polydata: vtkCellLocator.FindClosestPoint() non dispo en Python
#    cellId=0;
#    subId=0;
#    dist=0.0;
#    clospoint=[0,0,0]
#    m=[0,0,0]
#    cellLocator = vtk.vtkCellLocator();
#    cellLocator.SetDataSet(poly2);
#    cellLocator.BuildLocator();

    for cellId in range(refPoly.GetNumberOfCells()):
        cell = refPoly.GetCell(cellId)
        center = meshingTools.computeCenter(cell)
        # Find closest point on the surface defined by the reference data
        #m[0] = center[0]; m[1] = center[1]; m[2] = center[2];
        #cellLocator.FindClosestPoint(m, clospoint, cellId, subId, dist);
        dist = 0
        for cellId2 in range(poly2.GetNumberOfCells()):
            cell2 = refPoly.GetCell(cellId2)
            center2 = meshingTools.computeCenter(cell2)
            d = (center[0]-center2[0])*(center[0]-center2[0])+(center[1]-center2[1]) * \
                (center[1]-center2[1]) + \
                (center[2]-center2[2])*(center[2]-center2[2])
            if cellId2 == 0:
                dist = d
            if d < dist:
                dist = d
        darray.InsertValue(cellId, math.sqrt(dist))

    refPoly.GetCellData().AddArray(darray)
    return darray


def displayDistancesBetweenTwoPolysVectors(poly1, poly2, title='Distance between two polydatas'):
    """
    display poly1 and distance between points of poly2 and poly1 (represented by vectors)
    """
    # actor #1 : poly1
    mapper = vtk.vtkPolyDataMapper()
    mapper.SetInput(poly1)

    actor = vtk.vtkActor()
    actor.SetMapper(mapper)
    actor.GetProperty().SetColor(1, 0, 0)

    # actor#2 : hedgehog
    poly = poly1  # poly = duplicate(poly1)

    darray = vtk.vtkDoubleArray()
    darray.SetNumberOfComponents(3)
    nbpt = poly.GetPoints().GetNumberOfPoints()
    darray.SetNumberOfValues(nbpt)

    for k in range(0, nbpt):
        point1 = poly.GetPoint(k)
        point2 = poly2.GetPoint(k)
        DiX = point2[0] - point1[0]
        DiY = point2[1] - point1[1]
        DiZ = point2[2] - point1[2]
        darray.InsertTuple3(k, DiX, DiY, DiZ)

    poly.GetPointData().SetVectors(darray)

    hedgehog = vtk.vtkHedgeHog()
    hedgehog.SetInput(poly)
    hedgehog.SetScaleFactor(1.0)
    hedgehog.SetVectorModeToUseVector()

    hhMapper = vtk.vtkPolyDataMapper()
    hhMapper.SetInput(hedgehog.GetOutput())
    hhActor = vtk.vtkActor()
    hhActor.GetProperty().SetColor(0, 1, 0)
    hhActor.SetMapper(hhMapper)

    display3D((actor, hhActor), title)


def display2UG(UG1, UG2, title='Two Unstructured Grids'):
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
    actor2.GetProperty().SetColor(1, 0, 0)
    actor2.SetMapper(mapper2)

    display3D((actor1, actor2), title)


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


def displaySliceColors(image, sliceNo):
    """
    display 1 slice image in colors
    par rapport a la GUI, on a la couleur, et possibilite de faire un zoom
    (par contre, c'est pour 2D seulement)
    """

    lookup = vtk.vtkColorTransferFunction()
    lookup.AddRGBPoint(image.GetScalarRange()[0], 0, 0, 0)
    lookup.AddRGBPoint(image.GetScalarRange()[1], 0.0, 1.0, 0)
    lookup.AddRGBPoint(image.GetScalarRange()[1]*2, 1.0, 0.0, 0)

    viewer = vtk.vtkImageViewer2()
    viewer.SetInput(image)
    viewer.SetSlice(sliceNo)
    viewer.GetWindowLevel().SetLookupTable(lookup)

    iren = vtk.vtkRenderWindowInteractor()
    viewer.SetupInteractor(iren)
    viewer.Render()

    iren.Initialize()
    iren.Start()


def displaySliceContours(slice, numberOfContours=5, title='displaySliceContours'):
    '''
    Display slice and isovalue-contours
    '''
    imActor = vtk.vtkImageActor()
    imActor.SetInput(generalTools.castImage(slice, 'uchar'))

    skinExtractor = vtk.vtkContourFilter()
    skinExtractor.SetInput(slice)
    skinExtractor.GenerateValues(numberOfContours, slice.GetScalarRange())
    contour = skinExtractor.GetOutput()
    mapper = vtk.vtkPolyDataMapper()
    mapper.SetInput(contour)
    mapper.ScalarVisibilityOff()
    actor = vtk.vtkActor()
    actor.SetMapper(mapper)
    actor.GetProperty().SetColor(1, 0, 0)

    display3D((actor, imActor), title)


def create3Planes(image):
    xMin, xMax, yMin, yMax, zMin, zMax = image.GetWholeExtent()

    picker = vtk.vtkCellPicker()
    picker.SetTolerance(0.005)

    wX = vtk.vtkImagePlaneWidget()
    wX.DisplayTextOn()
    wX.SetInput(image)
    wX.SetPlaneOrientationToXAxes()
    wX.SetSliceIndex(old_div((xMax-xMin),2))
    wX.SetKeyPressActivationValue("x")
    wX.SetResliceInterpolateToLinear()
    wX.SetPicker(picker)
    wX.GetTextProperty().SetColor(1, 0, 0)
    wX.GetTextProperty().SetLineOffset(70)
    wX.GetTextProperty().BoldOn()
    wX.GetTextProperty().SetFontFamilyToArial()
    wX.GetTextProperty().SetFontSize(12)
    wX.GetPlaneProperty().SetColor(1, 0, 0)

    wY = vtk.vtkImagePlaneWidget()
    wY.DisplayTextOn()
    wY.SetInput(image)
    wY.SetPlaneOrientationToYAxes()
    wY.SetSliceIndex(old_div((yMax-yMin),2))
    wY.SetKeyPressActivationValue("y")
    wY.SetResliceInterpolateToLinear()
    wY.SetPicker(picker)
    wY.GetTextProperty().SetColor(1, 0, 0)
    wY.GetTextProperty().SetLineOffset(70)
    wY.GetTextProperty().BoldOn()
    wY.GetTextProperty().SetFontFamilyToArial()
    wY.GetTextProperty().SetFontSize(12)
    wY.GetPlaneProperty().SetColor(1, 1, 0)
    wY.SetLookupTable(wX.GetLookupTable())

    wZ = vtk.vtkImagePlaneWidget()
    wZ.DisplayTextOn()
    wZ.SetInput(image)
    wZ.SetPlaneOrientationToZAxes()
    wZ.SetSliceIndex(old_div((zMax-zMin),2))
    wZ.SetKeyPressActivationValue("z")
    wZ.SetResliceInterpolateToLinear()
    wZ.SetPicker(picker)
    wZ.GetTextProperty().SetColor(1, 0, 0)
    wZ.GetTextProperty().SetLineOffset(70)
    wZ.GetTextProperty().BoldOn()
    wZ.GetTextProperty().SetFontFamilyToArial()
    wZ.GetTextProperty().SetFontSize(12)
    wZ.GetPlaneProperty().SetColor(0, 0, 1)
    wZ.SetLookupTable(wX.GetLookupTable())

    return wX, wY, wZ


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
        if scalars == None:
            scalars = grid.GetCellData().GetScalars()
        if scalars == None:
            scalars = grid.GetPointData().GetArray(0)
            grid.GetPointData().SetScalars(scalars)
        if scalars == None:
            scalars = grid.GetCellData().GetArray(0)
            grid.GetCellData().SetScalars(scalars)
        if range == None:
            range = [scalars.GetRange()[0], .8*scalars.GetRange()[1]]
        mapper.SetScalarRange(*range)
        if colorMap == 'GrayScale':
            wl = vtk.vtkWindowLevelLookupTable()
        else:
            wl = vtk.vtkLookupTable()
            wl.SetScaleToLinear()
            wl.SetRange(*range)
            wl.SetHueRange(0.6667, 0.0)  # blue to red
            wl.Build()
        mapper.SetLookupTable(wl)
    else:
        mapper.ScalarVisibilityOff()
        actor.GetProperty().SetColor(.6, .6, .6)
    actor.SetMapper(mapper)
    if showEdges:
        actor.GetProperty().EdgeVisibilityOn()
    return actor


def createClipActor(grid, plane, showScalar=False, range=None, showEdges=False, colorMap='GrayScale'):
    clipMapper = vtk.vtkDataSetMapper()
    clipMapper.SetInput(grid)
    cutMapper = vtk.vtkDataSetMapper()
    clipActor = vtk.vtkActor()
    cutActor = vtk.vtkActor()
    if showScalar:
        scalars = grid.GetPointData().GetScalars()
        if scalars == None:
            scalars = grid.GetCellData().GetScalars()
        if scalars == None:
            scalars = grid.GetPointData().GetArray(0)
            grid.GetPointData().SetScalars(scalars)
        if scalars == None:
            scalars = grid.GetCellData().GetArray(0)
            grid.GetCellData().SetScalars(scalars)
        if range == None:
            range = [scalars.GetRange()[0], .8*scalars.GetRange()[1]]
        cutMapper.SetScalarRange(*range)
        clipMapper.SetScalarRange(*range)
        if colorMap == 'GrayScale':
            wl = vtk.vtkWindowLevelLookupTable()
        else:
            wl = vtk.vtkLookupTable()
            wl.SetScaleToLinear()
            wl.SetRange(*range)
            wl.SetHueRange(0.6667, 0.0)  # blue to red
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
    return [cutActor, clipActor]


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


def displayVolume(image, title='volume'):
    '''
    display a volume image - image must be a typical irm image with gray values comprised between 0 and 255
    '''
    image = generalTools.castImage(image, 'uchar')
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


def tetviewFromFile(fname="tmp.1.ele"):
    import os
    cmd = "tetview.exe %s" % (fname)
    # os.system(cmd)
    import subprocess
    subprocess.call(cmd, shell=True)


def displaySliceContours(image, th=None, res=1):
    '''
    display contours of slices of image; res == 1 -> all contours are displayed, image must be a segmeneted image
    '''
    import imagingTools
    aRenderer = vtk.vtkRenderer()
    aRenderer.SetBackground(1.0, 1.0, 1.0)

    i = 0
    if th == None:
        th = image.GetScalarRange()[1]
    while i < image.GetDimensions()[2]:
        slice = imagingTools.extract1Slice(image, i)
        skinExtractor = vtk.vtkContourFilter()
        skinExtractor.SetInput(slice)
        skinExtractor.SetValue(0, th)
        contour = skinExtractor.GetOutput()
        mapper = vtk.vtkPolyDataMapper()
        mapper.SetInput(contour)
        mapper.ScalarVisibilityOff()
        actor = vtk.vtkActor()
        actor.SetMapper(mapper)
        actor.GetProperty().SetColor(0, 0, 1)
        actor.GetProperty().SetLineWidth(2.0)
        aRenderer.AddActor(actor)
        i += res

    aRenderer.ResetCamera()
    cam1 = aRenderer.GetActiveCamera()
    cam1.Elevation(-85)
    cam1.SetViewUp(0, 0, 1)
    cam1.Azimuth(45)
    renWin = vtk.vtkRenderWindow()
    renWin.SetSize(700, 700)
    renWin.AddRenderer(aRenderer)
    renWin.Render()
    renWin.SetWindowName("Slice Contours")

    iren = vtk.vtkRenderWindowInteractor()
    iren.SetRenderWindow(renWin)

    iren.Initialize()
    iren.Start()


def displaySliceContours2(image, labels, res=1):
    '''
    display contours of slices of image; res == 1 -> all contours are displayed, image must be a segmeneted image
    '''
    import imagingTools
    aRenderer = vtk.vtkRenderer()
    aRenderer.SetBackground(1.0, 1.0, 1.0)

    i = 0
    while i < image.GetDimensions()[2]:
        slice = imagingTools.extract1Slice(image, i)

        for j in labels:
            skinExtractor = vtk.vtkContourFilter()
            skinExtractor.SetInput(slice)
            skinExtractor.SetValue(1, j)
            contour = skinExtractor.GetOutput()
            mapper = vtk.vtkPolyDataMapper()
            mapper.SetInput(contour)
            mapper.ScalarVisibilityOff()
            actor = vtk.vtkActor()
            actor.SetMapper(mapper)
            actor.GetProperty().SetColor(0, 0, 1)
            actor.GetProperty().SetLineWidth(2.5)
            aRenderer.AddActor(actor)
            i += res

    aRenderer.ResetCamera()
    cam1 = aRenderer.GetActiveCamera()
    cam1.Elevation(-85)
    cam1.SetViewUp(0, 0, 1)
    cam1.Azimuth(45)
    renWin = vtk.vtkRenderWindow()
    renWin.SetSize(700, 700)
    renWin.AddRenderer(aRenderer)
    renWin.Render()
    renWin.SetWindowName("Slice Contours")

    iren = vtk.vtkRenderWindowInteractor()
    iren.SetRenderWindow(renWin)

    iren.Initialize()
    iren.Start()


def createVolume(image):
    '''
    create volume of image - image must be a typical irm image with gray values comprised between 0 and 255
    '''
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
    volumeMapper = None
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
