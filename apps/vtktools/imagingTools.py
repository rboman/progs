#! /usr/bin/env python
# -*- coding: utf-8; -*-
# $Id: imagingTools.py 1388 2011-01-04 10:57:27Z papeleux $
# Vinciane d'Otreppe

# vtk filters to visualize images

import vtk
import sys
import generalTools
import math


def createEuclide(image):
    euclide = vtk.vtkImageEuclideanDistance()
    euclide.SetInput(image)
    euclide.SetDimensionality(3)
    euclide.ConsiderAnisotropyOn()
    euclide.SetAlgorithmToSaitoCached()
    euclide.InitializeOn()
    return euclide.GetOutput()


def createEuclide2D(image):
    euclide = vtk.vtkImageEuclideanDistance()
    euclide.SetInput(image)
    euclide.SetDimensionality(2)
    euclide.ConsiderAnisotropyOn()
    euclide.SetAlgorithmToSaitoCached()
    euclide.SetMaximumDistance(image.GetDimensions()[
                               0]*image.GetDimensions()[1])
    euclide.InitializeOn()
    return euclide.GetOutput()


def gaussianSmooth(image, stddev=3):

    gaussian = vtk.vtkImageGaussianSmooth()
    gaussian.SetInput(generalTools.castImage(image, coding='double'))
    gaussian.SetStandardDeviation(stddev)
    gaussian.SetDimensionality(3)
    gaussian.Update()
    return gaussian.GetOutput()


def createNegative(image):
    negfilter = vtk.vtkImageMathematics()
    negfilter.SetInput1(image)
    negfilter.SetOperationToInvert()
    negfilter.Update()
    return negfilter.GetOutput()


def addImages(image1, image2):
    addfilter = vtk.vtkImageMathematics()
    addfilter.SetInput2(image1)
    addfilter.SetInput1(image2)
    addfilter.SetOperationToAdd()
    addfilter.Update()
    return addfilter.GetOutput()


def subtractImages(image1, image2):
    addfilter = vtk.vtkImageMathematics()
    addfilter.SetInput1(image1)
    addfilter.SetInput2(image2)
    addfilter.SetOperationToSubtract()
    addfilter.Update()
    return addfilter.GetOutput()


def addConstant(image, c):
    addfilter = vtk.vtkImageMathematics()
    addfilter.SetInput(image)
    addfilter.SetConstantC(c)
    addfilter.SetOperationToAddConstant()
    addfilter.Update()
    return addfilter.GetOutput()


def replaceCbyK(image, c, k):
    replfilter = vtk.vtkImageMathematics()
    replfilter.SetInput(image)
    replfilter.SetConstantC(c)
    replfilter.SetConstantK(k)
    replfilter.SetOperationToReplaceCByK()
    replfilter.Update()
    return replfilter.GetOutput()


def multiplyByConstant(image, c):
    mulfilter = vtk.vtkImageMathematics()
    mulfilter.SetInput(image)
    mulfilter.SetConstantK(c)
    mulfilter.SetOperationToMultiplyByK()
    mulfilter.Update()
    return mulfilter.GetOutput()


def sqrtImage(image):
    sqrtfilter = vtk.vtkImageMathematics()
    sqrtfilter.SetInput(image)
    sqrtfilter.SetOperationToSquareRoot()
    sqrtfilter.Update()
    return sqrtfilter.GetOutput()


def openClose3D(image, openValue, closeValue, kernel):
    image2 = vtk.vtkImageOpenClose3D()
    image2.SetInput(image)
    image2.SetOpenValue(openValue)
    image2.SetCloseValue(closeValue)
    image2.SetKernelSize(kernel[0], kernel[1], kernel[2])
    image2.Update()
    return image2.GetOutput()


def extractScalarComponent(image):
    """
    extract first scalar component of image
    """
    extractImage = vtk.vtkImageExtractComponents()
    extractImage.SetInput(image)
    extractImage.SetComponents(1)
    extractImage.Update()
    return extractImage.GetOutput()


def dilate(image, kernel):
    dilate = vtk.vtkImageContinuousDilate3D()
    dilate.SetInput(image)
    dilate.SetKernelSize(kernel[0], kernel[1], kernel[2])
    dilate.Update()
    return dilate.GetOutput()


def permute(image, ax0, ax1, ax2):
    permute = vtk.vtkImagePermute()
    permute.SetInput(image)
    permute.SetFilteredAxes(ax0, ax1, ax2)
    return permute.GetOutput()


def erode(image, kernel):
    erode = vtk.vtkImageContinuousErode3D()
    erode.SetInput(image)
    erode.SetKernelSize(kernel[0], kernel[1], kernel[2])
    erode.Update()
    return erode.GetOutput()


def createSignedEuclideanDistanceMapEdge(segmentedImage):
    '''
    Euclidean distance maps of interior and exterior of segmentedImage are computed and added such that
    interior = positive values giving distance to edge
    exterior = negative values giving distance to edge
    '''
    negative = generalTools.castImage(createNegative(
        generalTools.castImage(segmentedImage, coding='uchar')), coding='double')
    euclide1 = createEuclide(generalTools.castImage(
        segmentedImage, coding='double'))
    euclide2 = createEuclide(generalTools.castImage(negative, coding='double'))
    segmentedMask = multiplyByConstant(thresholdByUpper(
        segmentedImage, 0.1, 1.0, 0), segmentedImage.GetSpacing()[0]/2)
    negativeMask = multiplyByConstant(thresholdByUpper(
        negative, 0.1, 1.0, 0), segmentedImage.GetSpacing()[0]/2)
    image1 = subtractImages(sqrtImage(euclide1), segmentedMask)
    image2 = subtractImages(sqrtImage(euclide2), negativeMask)
    imageSub = subtractImages(image1, image2)
    imageSub.Update()
    return imageSub


def createSignedEuclideanDistanceMap2D(segmentedImage):
    '''
    Euclidean distance maps of interior and exterior of segmentedImage are computed and added such that
    interior = positive values giving distance to edge
    exterior = negative values giving distance to edge
    '''
    negative = createNegative(
        generalTools.castImage(segmentedImage, coding='uchar'))
    euclide1 = createEuclide2D(
        generalTools.castImage(segmentedImage, coding='double'))
    euclide2 = createEuclide2D(
        generalTools.castImage(negative, coding='double'))
    image1 = sqrtImage(euclide1)
    image2 = sqrtImage(euclide2)
    imageSub = subtractImages(image2, image1)
    imageSub.Update()
    return imageSub


def createSignedEuclideanDistanceMapEdge2D(segmentedImage):
    '''
    Euclidean distance maps of interior and exterior of segmentedImage are computed and added such that
    interior = positive values giving distance to edge
    exterior = negative values giving distance to edge
    '''
    negative = createNegative(
        generalTools.castImage(segmentedImage, coding='uchar'))
    euclide1 = createEuclide2D(
        generalTools.castImage(segmentedImage, coding='double'))
    euclide2 = createEuclide2D(
        generalTools.castImage(negative, coding='double'))
    image1 = sqrtImage(euclide1)
    image2 = sqrtImage(euclide2)
    distMap = vtk.vtkImageData()
    distMap.SetDimensions(segmentedImage.GetDimensions())
    distMap.SetOrigin(segmentedImage.GetOrigin())
    distMap.SetSpacing(segmentedImage.GetSpacing())
    lx, ly, lz = distMap.GetDimensions()[0], distMap.GetDimensions()[
        1], distMap.GetDimensions()[2]
    for k in range(lz):
        for j in range(ly):
            for i in range(lx):
                val = image1.GetScalarComponentAsDouble(
                    i, j, k, 0)-image2.GetScalarComponentAsDouble(i, j, k, 0)
                if val > 0:
                    distMap.SetScalarComponentFromDouble(i, j, k, 0, val-0.5)
                else:
                    distMap.SetScalarComponentFromDouble(i, j, k, 0, val+0.5)
    return distMap


def translate3D(image, dx, dy, dz):
    t1 = vtk.vtkTransform()
    t1.Translate(dx, dy, dz)
    reslice = vtk.vtkImageReslice()
    reslice.SetInput(image)
    reslice.SetOutputDimensionality(3)
    reslice.SetResliceAxes(t1.GetInverse().GetMatrix())
    return reslice.GetOutput()


def translate2D(image, dx, dy):
    t1 = vtk.vtkTransform()
    t1.Translate(dx, dy, 0)
    reslice = vtk.vtkImageReslice()
    reslice.SetInput(image)
    reslice.SetOutputDimensionality(2)
    reslice.SetResliceAxes(t1.GetInverse().GetMatrix())
    out = reslice.GetOutput()
    out.Update()
    return out


def createEdgesDistanceMap(segmentedImage):
    '''
    Image of the edge (2 pixels large) of segmentedImage is created
    euclidean distance map of this edge image is computed
    '''
    edge = edgeImage(segmentedImage)
    negImage = createNegative(generalTools.castImage(edge, coding='uchar'))
    dilateImage = imagingTools.dilate(negImage, [2, 2, 2])
    negImageInverse = imagingTools.createNegative(dilateImage)
    distanceImage = imagingTools.createEuclide(negImageInverse)
    energyMap = imagingTools.sqrtImage(distanceImage)
    return energyMap


def getImplicitVolume(image):
    implicitVolume = vtk.vtkImplicitVolume()
    implicitVolume.SetVolume(image)
    implicitVolume.SetOutValue(-1000)
    implicitVolume.SetOutGradient(0, 0, 0)
    return implicitVolume


def flipImage(image, axe):
    flip = vtk.vtkImageFlip()
    flip.SetInput(image)
    flip.SetFilteredAxis(axe)
    flip.Update()
    return flip.GetOutput()


def thresholdByLower(image, threshold=1, invalue=255, outvalue=0):
    image2 = vtk.vtkImageThreshold()
    image2.SetInput(image)
    image2.ThresholdByLower(threshold)
    image2.SetInValue(invalue)
    image2.SetOutValue(outvalue)
    image2.Update()
    return image2.GetOutput()


def thresholdByUpper(image, threshold=1, invalue=255, outvalue=0):
    image2 = vtk.vtkImageThreshold()
    image2.SetInput(image)
    image2.ThresholdByUpper(threshold)
    image2.SetInValue(invalue)
    image2.SetOutValue(outvalue)
    image2.Update()
    return image2.GetOutput()


def isolateLabel(image, label):
    image1 = thresholdByUpper(image, label, 1, 0)
    image2 = thresholdByUpper(image, label+1, 1, 0)
    image3 = subtractImages(image2, image1)
    return image3


def changeExtent(image, extent):
    reslice = vtk.vtkImageReslice()
    reslice.SetInput(image)
    reslice.SetOutputSpacing(image.GetSpacing())
    origin0 = image.GetOrigin()[0] + extent[0]*image.GetSpacing()[0]
    origin1 = image.GetOrigin()[1] + extent[2]*image.GetSpacing()[1]
    origin2 = image.GetOrigin()[2] + extent[4]*image.GetSpacing()[2]
    reslice.SetOutputOrigin(origin0, origin1, origin2)
    outExtent = [0, extent[1]-extent[0], 0,
                 extent[3]-extent[2], 0, extent[5]-extent[4]]
    reslice.SetOutputExtent(outExtent)
    reslice.Update()
    return reslice.GetOutput()


def scaleImage(image, scaleX=None, scaleY=None, scaleZ=None):
    if (scaleX == None)or(scaleY == None)or(scaleZ == None):
        scale = max(abs(image.GetBounds()[1]-image.GetBounds()[0]), abs(image.GetBounds()[
                    3]-image.GetBounds()[2]), abs(image.GetBounds()[5]-image.GetBounds()[4]))
        scaleX, scaleY, scaleZ = 1./scale, 1./scale, 1./scale
    rescale = vtk.vtkImageChangeInformation()
    rescale.SetInput(image)
    rescale.SetOutputOrigin(0.0, 0.0, 0.0)
    rescale.SetSpacingScale(scaleX, scaleY, scaleZ)
    rescale.Update()
    return rescale.GetOutput()


def resliceWithCubicInterpolation(image, outSpacing):
    reslice = vtk.vtkImageReslice()
    reslice.SetInput(image)
    reslice.SetInterpolationModeToCubic()
    reslice.SetOutputSpacing([outSpacing[0], outSpacing[1], outSpacing[2]])
    reslice.SetOutputOrigin(image.GetOrigin())
    reslice.Update()
    return reslice.GetOutput()


def resliceWithNearestNeighbor(image, outSpacing):
    reslice = vtk.vtkImageReslice()
    reslice.SetInput(image)
    reslice.SetInterpolationModeToNearestNeighbor()
    reslice.SetOutputSpacing([outSpacing[0], outSpacing[1], outSpacing[2]])
    reslice.SetOutputOrigin(image.GetOrigin())
    reslice.Update()
    return reslice.GetOutput()


def resliceWithLinearInterpolation(image, outSpacing):
    reslice = vtk.vtkImageReslice()
    reslice.SetInput(image)
    reslice.SetInterpolationModeToLinear()
    reslice.SetOutputSpacing([outSpacing[0], outSpacing[1], outSpacing[2]])
    reslice.SetOutputOrigin(image.GetOrigin())
    reslice.BorderOn()
    reslice.Update()
    return reslice.GetOutput()


def createVtkSegmImageFromImplicitFunction(origin, spacing, dimensions, func1, func2=None):
    '''
    Creates a vtkImageData from a user-defined implicit function func
    '''
    imageData = vtk.vtkImageData()
    imageData.SetDimensions(dimensions)
    imageData.SetOrigin(origin)
    imageData.SetSpacing(spacing)
    imageData.SetScalarTypeToUnsignedChar()
    imageData.SetNumberOfScalarComponents(1)
    imageData.AllocateScalars()
    lx, ly, lz = imageData.GetDimensions()[0], imageData.GetDimensions()[
        1], imageData.GetDimensions()[2]
    for k in range(lz):
        for j in range(ly):
            for i in range(lx):
                pos = [imageData.GetOrigin()[0]+i*imageData.GetSpacing()[0], imageData.GetOrigin()[1] +
                       j*imageData.GetSpacing()[1], imageData.GetOrigin()[2]+k*imageData.GetSpacing()[2]]
                if func2:
                    if (func1.FunctionValue(pos) < 0) and (func2.FunctionValue(pos) < 0):
                        imageData.SetScalarComponentFromDouble(i, j, k, 0, 2)
                    elif (func1.FunctionValue(pos) < 0):
                        imageData.SetScalarComponentFromDouble(i, j, k, 0, 1)
                    else:
                        imageData.SetScalarComponentFromDouble(i, j, k, 0, 0)
                else:
                    if func1.FunctionValue(pos) < 0:
                        imageData.SetScalarComponentFromDouble(i, j, k, 0, 1)
                    else:
                        imageData.SetScalarComponentFromDouble(i, j, k, 0, 0)

    return imageData


def edgeImage(segmentedImage):
    '''
    Computes the boundary pixels of a segmented image.
    Implemented by ferrant in vtkLocalPython
    '''
    import vtkLocalPython
    inValue = segmentedImage.GetScalarRange()[1]
    filtreInitializeEdges = vtkLocalPython.vtkImageDTInitializeEdges()
    filtreInitializeEdges.SetInput(segmentedImage)
    filtreInitializeEdges.SetLabelOfTargetStructure(inValue)
    filtreInitializeEdges.SetBoundaryType(2)
    filtreInitializeEdges.Update()
    return filtreInitializeEdges.GetOutput()


def createMask(seg, image):
    '''
    combines a mask with an image. Non zero mask implies the output pixel will be the same as the image. If a mask pixel is zero, the output pixel is set to 'MaskedValue'==0.
    '''
    segUchar = castImage(seg, 'uchar')

    mask = vtk.vtkImageMask()
    mask.SetImageInput(image)
    mask.SetMaskInput(segUchar)
    mask.SetMaskedOutputValue(0.0)
    mask.NotMaskOff()
    mask.ReleaseDataFlagOff()
    mask.Update()
    return mask.GetOutput()


def extract1Slice(image, slice=30, dir=2):
    voi = vtk.vtkExtractVOI()
    voi.SetInput(image)
    extent = image.GetExtent()
    if dir == 0:
        voi.SetVOI(slice, slice, extent[2], extent[3], extent[4], extent[5])
    if dir == 1:
        voi.SetVOI(extent[0], extent[1], slice, slice, extent[4], extent[5])
    if dir == 2:
        voi.SetVOI(extent[0], extent[1], extent[2], extent[3], slice, slice)
    voi.Update()
    imagedata = voi.GetOutput()
    imagedata.SetOrigin(image.GetOrigin())
    imagedata.Update()
    return imagedata


def gradient2D(image):
    gradient = vtk.vtkImageGradient()
    gradient.SetInput(image)
    gradient.SetDimensionality(2)
    gradient.HandleBoundariesOn()
    gradient.Update()
    return gradient.GetOutput()


def gradientMagnitude2D(image):
    gradient = vtk.vtkImageGradientMagnitude()
    gradient.SetInput(image)
    gradient.SetDimensionality(2)
    gradient.HandleBoundariesOn()
    return gradient.GetOutput()


def linearInterpolation(image):
    '''
    Test - origin idem
    '''
    from math import floor
    outSpacing = [image.GetSpacing()[0], image.GetSpacing()[
        1], image.GetSpacing()[0]]
    outDimensions = [image.GetDimensions()[0], image.GetDimensions()[1], int(
        floor(((image.GetDimensions()[2]-1)*image.GetSpacing()[2]) / outSpacing[2]))+1]
    interpolatedImage = vtk.vtkImageData()
    interpolatedImage.SetDimensions(outDimensions)
    interpolatedImage.SetOrigin(image.GetOrigin())
    interpolatedImage.SetSpacing(outSpacing)
    interpolatedImage.SetScalarTypeToDouble()

    generalTools.printVtkImageInfo(image, 'image.vtk')
    generalTools.printVtkImageInfo(interpolatedImage, 'interpolatedImage.vtk')

    lx, ly, lz = interpolatedImage.GetDimensions()[0], interpolatedImage.GetDimensions()[
        1], interpolatedImage.GetDimensions()[2]
    if (((image.GetDimensions()[2]-1) * image.GetSpacing()[2]) == ((outDimensions[2]-1) * outSpacing[2])):
        lz = lz-1
        for j in range(ly):
            for i in range(lx):
                interpolatedImage.SetScalarComponentFromDouble(
                    i, j, lz, 0, image.GetScalarComponentAsDouble(i, j, image.GetDimensions()[2]-1, 0))
    for k in range(lz):
        interpolatedSliceHeight = interpolatedImage.GetOrigin(
        )[2] + k * interpolatedImage.GetSpacing()[2]
        imageSliceDownIndice = int(
            floor((interpolatedSliceHeight - image.GetOrigin()[2])/image.GetSpacing()[2]))
        imageSliceUpIndice = imageSliceDownIndice+1
        imageSliceDownHeightDiff = interpolatedSliceHeight - \
            (image.GetOrigin()[2] +
             imageSliceDownIndice * image.GetSpacing()[2])
        imageSliceUpHeightDiff = image.GetOrigin(
        )[2] + imageSliceUpIndice * image.GetSpacing()[2] - interpolatedSliceHeight
        imageDistanceBetweenSlices = imageSliceDownHeightDiff + imageSliceUpHeightDiff
        for j in range(ly):
            for i in range(lx):
                d = (image.GetScalarComponentAsDouble(i, j, imageSliceDownIndice, 0)*imageSliceUpHeightDiff +
                     image.GetScalarComponentAsDouble(i, j, imageSliceUpIndice, 0)*imageSliceDownHeightDiff)/imageDistanceBetweenSlices
                interpolatedImage.SetScalarComponentFromDouble(i, j, k, 0, d)

    return interpolatedImage


def createTestImage():
    image = vtk.vtkImageData()
    image.SetDimensions([100, 100, 100])
    image.SetOrigin([-50, -50, -50])
    image.SetSpacing([1, 1, 1])
    image.SetScalarTypeToDouble()

    range1 = [image.GetDimensions()[0]/3, image.GetDimensions()[0]*2/3, image.GetDimensions()[1]/3,
              image.GetDimensions()[1]*2/3, image.GetDimensions()[2]/3, image.GetDimensions()[2]*2/3]
    range2 = [image.GetDimensions()[0]*4/10, image.GetDimensions()[0]*6/10, image.GetDimensions()[1]
              * 4/10, image.GetDimensions()[1]*6/10, image.GetDimensions()[2]/3, image.GetDimensions()[2]*6/10]

    for k in range(image.GetDimensions()[2]):
        for j in range(image.GetDimensions()[1]):
            for i in range(image.GetDimensions()[0]):
                if(i > range2[0] and i < range2[1] and j > range2[2] and j < range2[3] and k > range2[4] and k < range2[5]):
                    image.SetScalarComponentFromDouble(i, j, k, 0, 2)
                elif(i > range1[0] and i < range1[1] and j > range1[2] and j < range1[3] and k > range1[4] and k < range1[5]):
                    image.SetScalarComponentFromDouble(i, j, k, 0, 1)
                else:
                    image.SetScalarComponentFromDouble(i, j, k, 0, 0)

    return image


def createEllipsoid(extent=(0, 99, 0, 99, 0, 99), center=(50, 50, 50), radius=(20, 35, 25), coding='uchar', values=(255, 0)):
    """
    create a stupid ellipsoid (test purpose)
    """
    ellipse = vtk.vtkImageEllipsoidSource()
    ellipse.SetWholeExtent(*extent)
    ellipse.SetCenter(*center)
    ellipse.SetRadius(*radius)
    if coding == 'uchar':
        ellipse.SetOutputScalarTypeToUnsignedChar()
    elif coding == 'ushort':
        ellipse.SetOutputScalarTypeToUnsignedShort()
    else:
        print 'bad coding : %s' % coding
        sys.exit(1)
    ellipse.SetInValue(values[0])
    ellipse.SetOutValue(values[1])
    ellipse.Update()
    return ellipse.GetOutput()


def createSphere(extent=(0, 99, 0, 99, 0, 99), center=(50, 50, 50), radius=20, coding='uchar', values=(255, 0)):
    """
    create a stupid sphere (test purpose)
    """
    return createEllipsoid(extent, center, (radius, radius, radius), coding, values)


def createContourPolydata(image, value=(0, 2), fangle=60):
    skinExtractor = vtk.vtkContourFilter()
    skinExtractor.SetInput(image)
    skinExtractor.SetValue(*value)
    skinNormals = vtk.vtkPolyDataNormals()
    skinNormals.SetInputConnection(skinExtractor.GetOutputPort())
    skinNormals.SetFeatureAngle(fangle)
    skinNormals.Update()
    return skinNormals.GetOutput()


def createContourActor(image, value=(0, 2), fangle=60):
    polydata = createContourPolydata(image, value, fangle)
    skinMapper = vtk.vtkPolyDataMapper()
    skinMapper.SetInput(polydata)
    skinMapper.ScalarVisibilityOff()
    skin = vtk.vtkActor()
    skin.SetMapper(skinMapper)
    return skin


def createWarpPolyData(image, scale=1.0):
    geometry = vtk.vtkImageDataGeometryFilter()
    geometry.SetInput(image)
    warp = vtk.vtkWarpScalar()
    warp.SetInput(geometry.GetOutput())
    warp.SetScaleFactor(scale)
    warp.Update()
    return warp.GetOutput()


def extractContourPointsFromSegementedImage(image, th=(0, 1), filename="contourPoints"):
    skinExtractor = vtk.vtkContourFilter()
    skinExtractor.SetInput(image)
    skinExtractor.SetValue(*th)
    skinExtractor.Update()
    contour = skinExtractor.GetOutput()
    n = contour.GetNumberOfCells()
    count = 0
    file = open(filename, 'w')
    file.write(
        "# contours points created via extractContourPointsOfSegementedImage\n")
    for cid in range(n):
        cell = contour.GetCell(cid)
        m = cell.GetNumberOfPoints()
        for pid in range(m):
            point = contour.GetPoint(cell.GetPointId(pid))
            count += 1
            file.write("%d %f %f %f\n" % (count, point[0], point[1], point[2]))
    file.close()


def extractContourPointsAndNormalsFromSegmentedImage(image, thres=1, prohibitedneighbours=[1, 3, 4], stddev=3, filename="contourPoints.pwn"):
    gaussianImage = gaussianSmooth(image, 3)
    sp = image.GetSpacing()
    generalTools.saveVtkImage("contourPointsImageSmoothed.vtk", gaussianImage)
    gaussianVolume = getImplicitVolume(gaussianImage)
    file = open(filename, 'w')
    file.write("                                      \n")
    nbOfPoints = 0
    #from renderingTools import displaySliceContours
    # displaySliceContours(gaussianImage,th,1.0)
    for slicenbr in range(image.GetExtent()[4], image.GetExtent()[5]):
        slice = extract1Slice(image, slicenbr)
        skinExtractor = vtk.vtkContourFilter()
        skinExtractor.SetInput(slice)
        skinExtractor.SetValue(0, thres)         # CHANGE !!
        skinExtractor.Update()
        contour = skinExtractor.GetOutput()
        n = contour.GetNumberOfCells()
        for cid in range(n):
            cell = contour.GetCell(cid)
            m = cell.GetNumberOfPoints()
            for pid in range(m):
                point = contour.GetPoint(cell.GetPointId(pid))
                ijk = [0, 0, 0]
                pcoords = [0, 0, 0]
                image.ComputeStructuredCoordinates(point, ijk, pcoords)
                vals = [image.GetScalarComponentAsDouble(ijk[0]-1, ijk[1], ijk[2], 0),
                        image.GetScalarComponentAsDouble(
                            ijk[0]+1, ijk[1], ijk[2], 0),
                        image.GetScalarComponentAsDouble(
                            ijk[0], ijk[1]-1, ijk[2], 0),
                        image.GetScalarComponentAsDouble(
                            ijk[0], ijk[1]+1, ijk[2], 0),
                        image.GetScalarComponentAsDouble(
                            ijk[0], ijk[1], ijk[2]-1, 0),
                        image.GetScalarComponentAsDouble(ijk[0], ijk[1], ijk[2]+1, 0)]
                add = True
                for v in vals:
                    if v in prohibitedneighbours:
                        add = False
                if add:
                    n = [0, 0, 0]
                    gaussianVolume.EvaluateGradient(point, n)
                    norm = math.sqrt(n[0]*n[0]+n[1]*n[1]+n[2]*n[2])
                    if norm != 0:
                        n = [n[0]/norm, n[1]/norm, n[2]/norm]
                        nbOfPoints += 1
                        file.write("%f %f %f %f %f %f\n" % (
                            point[0], point[1], point[2], n[0], n[1], n[2]))
                    else:
                        print "normal does not exists"
    file.close()
    file2 = open(filename, 'r+')
    file2.write("%d" % nbOfPoints)
    file2.close()


def extractContourPointsAndNormalsFromImage1Domain(image, th, filename):

    # Gaussian image
    gaussianImage = gaussianSmooth(image, 3)
    generalTools.saveVtkImage("contourPointsImageSmoothed.vtk", gaussianImage)
    gaussianVolume = imagingTools.getImplicitVolume(gaussianImage)

    # Output File
    file = open(filename, 'w')
    file.write("                                      \n")
    nbOfPoints = 0

    for slicenbr in range(image.GetExtent()[4], image.GetExtent()[5]):
        # extract slice
        slice = imagingTools.extract1Slice(image, slicenbr)

        # extract contour (threshold)
        skinExtractor = vtk.vtkContourFilter()
        skinExtractor.SetInput(slice)
        skinExtractor.SetValue(0, th)
        skinExtractor.Update()
        contour = skinExtractor.GetOutput()

        # get contour points and take corresponding normals in gaussianimage
        n = contour.GetNumberOfCells()
        for cid in range(n):
            cell = contour.GetCell(cid)
            m = cell.GetNumberOfPoints()
            for pid in range(m):
                point = contour.GetPoint(cell.GetPointId(pid))
                n = [0, 0, 0]
                gaussianVolume.EvaluateGradient(point, n)
                norm = math.sqrt(n[0]*n[0]+n[1]*n[1]+n[2]*n[2])
                n = [n[0]/norm, n[1]/norm, n[2]/norm]
                nbOfPoints += 1
                file.write("%f %f %f %f %f %f\n" %
                           (point[0], point[1], point[2], n[0], n[1], n[2]))

    # add vertical normals in first and last slices
    for iz in range(image.GetExtent()[4]+1, image.GetExtent()[5]-1):
        for iy in range(image.GetExtent()[2], image.GetExtent()[3]):
            for ix in range(image.GetExtent()[0], image.GetExtent()[1]):
                valsup = image.GetScalarComponentAsDouble(ix, iy, iz-1, 0)
                valinf = image.GetScalarComponentAsDouble(ix, iy, iz+1, 0)
                val = image.GetScalarComponentAsDouble(ix, iy, iz, 0)

                if (((valsup == th) and (val == 0)) or ((valinf == th) and (val == 0))):
                    pos = [image.GetOrigin()[0]+ix*image.GetSpacing()[0], image.GetOrigin()[1] +
                           iy*image.GetSpacing()[1], image.GetOrigin()[2]+iz*image.GetSpacing()[2]]
                    n = [0, 0, 0]
                    gaussianVolume.EvaluateGradient(pos, n)
                    norm = math.sqrt(n[0]*n[0]+n[1]*n[1]+n[2]*n[2])
                    if norm != 0:
                        n = [n[0]/norm, n[1]/norm, n[2]/norm]
                        if abs(n[0]) == 0.0 and abs(n[1]) == 0.0:
                            nbOfPoints += 1
                            file.write("%f %f %f %f %f %f\n" % (
                                pos[0], pos[1], pos[2], n[0], n[1], n[2]))

    file.close()
    file2 = open(filename, 'r+')
    file2.write("%d" % nbOfPoints)
    file2.close()


def extractContourPointsAndNormalsFromImageMultipleDomains(image, th=[], filename=[]):

    # BROUILLON !!!!!!!!!!!

    # Gaussian image
    gaussianImage = gaussianSmooth(image, 3)
    generalTools.saveVtkImage("contourPointsImageSmoothed.vtk", gaussianImage)
    gaussianVolume = imagingTools.getImplicitVolume(gaussianImage)

    for i in range(0, len(th)):

        # Output File
        file = open(filename[i], 'w')
        file.write("                                      \n")
        nbOfPoints = 0

        for slicenbr in range(image.GetExtent()[4], image.GetExtent()[5]):
            # extract slice
            slice = imagingTools.extract1Slice(image, slicenbr)

            # extract contour (threshold)
            skinExtractor = vtk.vtkContourFilter()
            skinExtractor.SetInput(slice)

            # extraction de tous les contours
            skinExtractor.SetValue(0, th)
            skinExtractor.Update()
            contour = skinExtractor.GetOutput()

            # get contour points and take corresponding normals in gaussianimage
            n = contour.GetNumberOfCells()
            for cid in range(n):
                cell = contour.GetCell(cid)
                m = cell.GetNumberOfPoints()
                for pid in range(m):
                    point = contour.GetPoint(cell.GetPointId(pid))
                    n = [0, 0, 0]
                    gaussianVolume.EvaluateGradient(point, n)
                    norm = math.sqrt(n[0]*n[0]+n[1]*n[1]+n[2]*n[2])
                    n = [n[0]/norm, n[1]/norm, n[2]/norm]
                    nbOfPoints += 1
                    file.write("%f %f %f %f %f %f\n" %
                               (point[0], point[1], point[2], n[0], n[1], n[2]))

        # add vertical normals in first and last slices
        for iz in range(image.GetExtent()[4]+1, image.GetExtent()[5]-1):
            for iy in range(image.GetExtent()[2], image.GetExtent()[3]):
                for ix in range(image.GetExtent()[0], image.GetExtent()[1]):
                    valsup = image.GetScalarComponentAsDouble(ix, iy, iz-1, 0)
                    valinf = image.GetScalarComponentAsDouble(ix, iy, iz+1, 0)
                    val = image.GetScalarComponentAsDouble(ix, iy, iz, 0)

                    if (((valsup == th) and (val == 0)) or ((valinf == th) and (val == 0))):
                        pos = [image.GetOrigin()[0]+ix*image.GetSpacing()[0], image.GetOrigin()[
                            1]+iy*image.GetSpacing()[1], image.GetOrigin()[2]+iz*image.GetSpacing()[2]]
                        n = [0, 0, 0]
                        gaussianVolume.EvaluateGradient(pos, n)
                        norm = math.sqrt(n[0]*n[0]+n[1]*n[1]+n[2]*n[2])
                        if norm != 0:
                            n = [n[0]/norm, n[1]/norm, n[2]/norm]
                            if abs(n[0]) == 0.0 and abs(n[1]) == 0.0:
                                nbOfPoints += 1
                                file.write("%f %f %f %f %f %f\n" % (
                                    pos[0], pos[1], pos[2], n[0], n[1], n[2]))

        file.close()
        file2 = open(filename, 'r+')
        file2.write("%d" % nbOfPoints)
        file2.close()
