#! /usr/bin/env python3
# -*- coding: utf-8 -*-
#
#   Copyright 2006-2017 Romain Boman
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

#
# vtkTools - VTK/Python interface by RoBo
#
# June 2006


from __future__ import print_function
from __future__ import division
from builtins import str
from builtins import range
from past.utils import old_div
import vtk
import sys

def loadRawImage(name, extent=(0,255,0,255,0,59), spacing=(1,1,1), coding='uchar',byteorder='little'):
    """
    load a Raw Image (.img,.raw) into a vtkImageData
    """
    reader = vtk.vtkImageReader()
    reader.SetDataExtent(*extent)
    reader.SetDataSpacing(*spacing)
    
    import os, stat
    if stat.S_ISDIR(os.stat(name)[stat.ST_MODE]):
        reader.SetFilePrefix(name)
        reader.SetFilePattern ("%s/I.%03d")
    else:    
        reader.SetFileDimensionality(3)
        reader.SetFileName(name) # !! pas d'espace dans le nom de fichier !!
    if byteorder=='little':
        reader.SetDataByteOrderToLittleEndian()
    else:
        reader.SetDataByteOrderToBigEndian()
    if coding=='uchar':
        reader.SetDataScalarTypeToUnsignedChar()
    elif coding=='ushort':
        reader.SetDataScalarTypeToUnsignedShort()
    else:
        print('bad coding : %s' % coding)
        sys.exit(1)
    reader.Update()
    return reader.GetOutput()

def saveRawImage(name, image):
    """
    save a Raw Image (.img,.raw) from a vtkImageData
    """
    writer = vtk.vtkImageWriter()
    writer.SetFileName(name)
    writer.SetInput(image)
    writer.SetFileDimensionality(3)
    writer.Write()
    
def loadVtkImage(name):
    """
    load a VTK Image (.vtk) into a vtkImageData (vtkStructuredPoints)
    """
    reader = vtk.vtkStructuredPointsReader()
    reader.SetFileName(name)
    reader.Update()    
    return reader.GetOutput()

def saveVtkImage(name, image, asciiorbin='binary'):
    """
    save a VTK Image (.vtk) from a vtkImageData (binary)
    """
    writer = vtk.vtkStructuredPointsWriter();
    writer.SetHeader("# cree par RoBo")
    if asciiorbin=='binary':
        writer.SetFileTypeToBinary()
    else:
        writer.SetFileTypeToASCII()    
    writer.SetInput(image);
    writer.SetFileName(name)
    writer.Write() # really slow if ASCII!!!
    
def loadVtkImageXML(name):
    """
    load a VTK Image (.vti) into a vtkImageData (vtkStructuredPoints)
    """
    reader = vtk.vtkXMLImageDataReader()
    reader.SetFileName(name)
    reader.Update()    
    return reader.GetOutput()

def saveVtkImageXML(name, image, asciiorbin='binary'):
    """
    save a VTK Image (.vti) from a vtkImageData (binary)
    """
    writer = vtk.vtkXMLImageDataWriter();
    if asciiorbin=='binary':
        writer.SetDataModeToBinary()
    else:
        writer.SetDataModeToAscii()
    writer.SetInput(image);
    writer.SetFileName(name)
    writer.Write()

def extractOneSlice(image, slice=30):
    voi = vtk.vtkExtractVOI()
    voi.SetInput(image)
    extent = image.GetExtent()
    voi.SetVOI(extent[0],extent[1],extent[2],extent[3],slice,slice)
    return voi.GetOutput()    
    
def displayOneSlice(image, slice=30, window=255, level=127):
    viewer = vtk.vtkImageViewer()
    viewer.SetInput(image)
    viewer.SetZSlice(slice)
    viewer.SetColorWindow(window)
    viewer.SetColorLevel(level)
    viewer.Render()

    interact = vtk.vtkRenderWindowInteractor()
    viewer.SetupInteractor(interact)
    interact.Initialize()
    interact.Start()

def createContourPolydata(image, value=(0,2), fangle=60):
    skinExtractor = vtk.vtkContourFilter()
    skinExtractor.SetInput(image)
    skinExtractor.SetValue(*value)
    
    skinNormals = vtk.vtkPolyDataNormals()
    skinNormals.SetInputConnection(skinExtractor.GetOutputPort())
    skinNormals.SetFeatureAngle(fangle)
    return skinNormals.GetOutput()   

def createContourActor(image, value=(0,2), fangle=60):
    polydata = createContourPolydata(image, value, fangle)
    skinMapper = vtk.vtkPolyDataMapper()
    skinMapper.SetInput(polydata)
    skinMapper.ScalarVisibilityOff()
    skin = vtk.vtkActor()
    skin.SetMapper(skinMapper)
    return skin

def createPolyDataActor(polydata, showScalar=False, range=None):
    mapper = vtk.vtkPolyDataMapper()
    mapper.SetInput(polydata)
    if showScalar:
        wl = vtk.vtkWindowLevelLookupTable()
        if range==None:
            range = polydata.GetPointData().GetScalars().GetRange()
        mapper.SetScalarRange(*range)
        mapper.SetLookupTable(wl)
    else:
        mapper.ScalarVisibilityOff()
    
    actor = vtk.vtkActor()
    actor.SetMapper(mapper)
    return actor

def createOutlineActor(image):
    outlineData = vtk.vtkOutlineFilter()
    outlineData.SetInput(image)
    mapOutline = vtk.vtkPolyDataMapper()
    mapOutline.SetInputConnection(outlineData.GetOutputPort())
    outline = vtk.vtkActor()
    outline.SetMapper(mapOutline)
    outline.GetProperty().SetColor(0, 0, 0)
    return outline
  
def createWarpPolyData(image, scale=1.0):
    geometry = vtk.vtkImageDataGeometryFilter()
    geometry.SetInput(image)
    warp = vtk.vtkWarpScalar()
    warp.SetInput(geometry.GetOutput())
    warp.SetScaleFactor(scale)
    warp.Update()
    return warp.GetOutput()
    
def display3D(actors):
    aRenderer = vtk.vtkRenderer()
    aRenderer.SetBackground(0.1, 0.2, 0.4) 
       
    renWin = vtk.vtkRenderWindow()
    renWin.SetSize(640, 480)    
    renWin.AddRenderer(aRenderer)
    iren = vtk.vtkRenderWindowInteractor()
    iren.SetRenderWindow(renWin)

    for actor in actors:
        aRenderer.AddActor(actor)

    iren.Initialize()
    renWin.Render()
    iren.Start()


def readImageScalarRange(filename, extent=(0,255,0,255,0,59), coding='ushort'):
    """
    display scalar range of a file (no vtk call - python only)
    """
    import struct
    l=(extent[1]-extent[0]+1)*(extent[3]-extent[2]+1)*(extent[5]-extent[4]+1)
    typeDataIn=str(l)
    if coding=='ushort':
        typeDataIn=typeDataIn+'H' # unsigned short
    elif coding=='uchar':
        typeDataIn=typeDataIn+'B' # unsigned char
    else:
        print('bad coding : %s' % coding)
        sys.exit(1)
    # read file
    file = open(filename, 'rb')
    allfile = file.read()
    file.close()

    value = struct.unpack(typeDataIn, allfile[0:struct.calcsize(typeDataIn)])
    range = (min(value), max(value))
    return range

def printOneLineOfData(filename, slice=30, line=128, extent=(0,255,0,255,0,59), coding='ushort'):
    
    import struct

    typeDataIn=''
    if coding=='ushort':
        typeDataIn=typeDataIn+'H' # unsigned short
    elif coding=='uchar':
        typeDataIn=typeDataIn+'B' # unsigned char
    else:
        print('bad coding : %s' % coding)
        sys.exit(1)

    # read file
    file = open(filename, 'rb')
    allfile = file.read()
    file.close()

    # display a line on the screen
    sizIn = struct.calcsize(typeDataIn)
    
    lx=extent[1]-extent[0]+1
    ly=extent[3]-extent[2]+1
    lz=extent[5]-extent[4]+1
    
    start=sizIn*(lx*ly*slice+lx*line)
    for j in range(extent[0],extent[1]):
        value = struct.unpack(typeDataIn, allfile[start:start+sizIn])[0]
        print(value, end=' ')
        start=start+sizIn
    print('')

def loadGenesisImage(directory, range=(1,60)):
    reader = vtk.vtkGESignaReader()
    reader.SetDataExtent(0,0,0,0,range[0],range[1])
    #reader.SetFileNameSliceOffset(1)
    reader.SetFilePrefix(directory)
    reader.SetFilePattern ("%s/I.%03d")
    #print 'header size =', reader.GetHeaderSize()
    #print 'format =', reader.GetDescriptiveName()
    reader.Update()
    return reader.GetOutput()

def convert16to8bits(name16, name8, extent=(0,255,0,255,0,59)):
    import struct
    l=(extent[1]-extent[0]+1)*(extent[3]-extent[2]+1)*(extent[5]-extent[4]+1)
    typeDataIn=str(l)+'H' # unsigned short
    typeDataOut=str(l)+'B' # unsigned char

    # read file
    file = open(name16, 'rb')
    allfile = file.read()
    file.close()

    # convert file
    outfile = open(name8, 'wb')

    sizIn  = struct.calcsize(typeDataIn)
    sizOut = struct.calcsize(typeDataOut)

    start=0
    value = struct.unpack(typeDataIn, allfile[start:start+sizIn])
    value = (typeDataOut,)+value
    outfile.write(struct.pack(*value))
    outfile.close()

def off2vtk(name="ellipse.off"):
    """
    read an OOGL (isosurf output) file into memory (polydata)
    """
    inFile = open(name, 'r')
    tag = inFile.readline()
    if tag[0:4]!="NOFF":
        print("bad format!")
        sys.exit()
    line = inFile.readline().split()
    nbnod = int(line[0])
    nbelm = int(line[1])
    #print 'reading',nbnod,'nodes and',nbelm,'elements'

    polydata = vtk.vtkPolyData()
    points   = vtk.vtkPoints()
    polys    = vtk.vtkCellArray()

    for i in range(0,nbnod):
        line=inFile.readline().split()
        points.InsertPoint(i, float(line[0]), float(line[1]), float(line[2]))

    for i in range(0,nbelm):
        line=inFile.readline().split()
        if int(line[0])!=3:
            print("bad elem (%d nodes)" % int(line[0]))
            sys.exit()
        n1=int(line[1])
        n2=int(line[2])
        n3=int(line[3])
        #polys.InsertNextCell(3, [n1, n2, n3]) # marche pas sous python
        polys.InsertNextCell(3)
        polys.InsertCellPoint(n1)
        polys.InsertCellPoint(n2)
        polys.InsertCellPoint(n3)

    polydata.SetPoints(points)
    polydata.SetPolys(polys)
    return polydata
    

def loadPolyData(name):
    reader = vtk.vtkPolyDataReader()
    reader.SetFileName(name)
    reader.Update()
    return reader.GetOutput()
    
def savePolyData(name, polydata, asciiorbin='binary'):
    """
    save a polydata to disk in vtk format
    """
    writer = vtk.vtkPolyDataWriter()
    writer.SetFileName(name)
    if asciiorbin=='binary':
        writer.SetFileTypeToBinary()
    else:
        writer.SetFileTypeToASCII()    
    writer.SetInput(polydata)
    writer.Write()
    
def loadPolyDataXML(name):
    """
    load a VTK PolyData (.vtp) into a vtkPolyData using XML format
    """
    reader = vtk.vtkXMLPolyDataReader()
    reader.SetFileName(name)
    reader.Update()    
    return reader.GetOutput()

def savePolyDataXML(name, image, asciiorbin='binary'):
    """
    save a VTK PolyData (.vtp) from a vtkPolyData using XML format
    """
    writer = vtk.vtkXMLPolyDataWriter()
    print('asciiorbin=', asciiorbin.get())
    if asciiorbin=='binary':
        writer.SetDataModeToBinary() # marche pas...
    else:
        writer.SetDataModeToAscii()
    writer.SetInput(image);
    writer.SetFileName(name)
    writer.Write()

def createEuclide(image):
    euclide = vtk.vtkImageEuclideanDistance()
    euclide.SetInput(image)
    euclide.SetDimensionality(3)
    euclide.SetAlgorithmToSaitoCached() 
    #euclide.InitializeOn()
    return euclide.GetOutput()

def createNegative(image):
    negfilter = vtk.vtkImageMathematics()
    negfilter.SetInput1(image)
    negfilter.SetOperationToInvert()
    return negfilter.GetOutput()
    
def addImages(image1, image2):
    addfilter = vtk.vtkImageMathematics()
    addfilter.SetInput2(image1)
    addfilter.SetInput1(image2)
    addfilter.SetOperationToAdd()
    return addfilter.GetOutput()
    
def createEllipsoid(extent=(0,99,0,99,0,99), center=(50, 50, 50), radius=(20, 35, 25), coding='uchar',values=(255,0)):
    """
    create a stupid ellipsoid (test purpose)
    """
    ellipse = vtk.vtkImageEllipsoidSource();
    ellipse.SetWholeExtent(*extent); 
    ellipse.SetCenter(*center);
    ellipse.SetRadius(*radius);
    if coding=='uchar':
        ellipse.SetOutputScalarTypeToUnsignedChar();
    elif coding=='ushort':
        ellipse.SetOutputScalarTypeToUnsignedShort();
    else:
        print('bad coding : %s' % coding)
        sys.exit(1)
    ellipse.SetInValue(values[0]);
    ellipse.SetOutValue(values[1]);
    ellipse.Update()
    return ellipse.GetOutput()
 
def createSphere(extent=(0,99,0,99,0,99), center=(50, 50, 50), radius=20, coding='uchar',values=(255,0)):
    """
    create a stupid sphere (test purpose)
    """
    return createEllipsoid(extent, center, (radius,radius,radius), coding, values)

def create3Planes(image, window, level):
    xMin, xMax, yMin, yMax, zMin, zMax = image.GetWholeExtent()

    picker = vtk.vtkCellPicker()
    picker.SetTolerance(0.005)

    planeWidgetX = vtk.vtkImagePlaneWidget()
    planeWidgetX.DisplayTextOn()
    planeWidgetX.SetInput(image)
    planeWidgetX.SetPlaneOrientationToXAxes()
    planeWidgetX.SetSliceIndex(old_div((xMax-xMin),2))
    planeWidgetX.SetKeyPressActivationValue("x")
    planeWidgetX.SetPicker(picker)
    prop1 = planeWidgetX.GetPlaneProperty()
    prop1.SetColor(1, 0, 0)

    planeWidgetY = vtk.vtkImagePlaneWidget()
    planeWidgetY.DisplayTextOn()
    planeWidgetY.SetInput(image)
    planeWidgetY.SetPlaneOrientationToYAxes()
    planeWidgetY.SetSliceIndex(old_div((yMax-yMin),2))
    planeWidgetY.SetKeyPressActivationValue("y")
    planeWidgetY.SetPicker(picker)
    prop2 = planeWidgetY.GetPlaneProperty()
    prop2.SetColor(1, 1, 0)
    planeWidgetY.SetLookupTable(planeWidgetX.GetLookupTable())

    planeWidgetZ = vtk.vtkImagePlaneWidget()
    planeWidgetZ.DisplayTextOn()
    planeWidgetZ.SetInput(image)
    planeWidgetZ.SetPlaneOrientationToZAxes()
    planeWidgetZ.SetSliceIndex(old_div((zMax-zMin),2))
    planeWidgetZ.SetKeyPressActivationValue("z")
    planeWidgetZ.SetPicker(picker)
    prop3 = planeWidgetZ.GetPlaneProperty()
    prop3.SetColor(0, 0, 1)
    planeWidgetZ.SetLookupTable(planeWidgetX.GetLookupTable())
    return planeWidgetX,planeWidgetY,planeWidgetZ 


def view3Planes(image, window, level):
    planeWidgetX,planeWidgetY,planeWidgetZ = create3Planes(image, window, level)
    aRenderer = vtk.vtkRenderer()
    aRenderer.SetBackground(0.1, 0.2, 0.4) 

    renWin = vtk.vtkRenderWindow()
    renWin.SetSize(640, 480)    
    renWin.AddRenderer(aRenderer)
    iren = vtk.vtkRenderWindowInteractor()
    iren.SetRenderWindow(renWin)

    planeWidgetX.SetInteractor(iren)
    planeWidgetX.On()
    planeWidgetY.SetInteractor(iren)
    planeWidgetY.On()
    planeWidgetZ.SetInteractor(iren)
    planeWidgetZ.On()

    outline = createOutlineActor(image)
    aRenderer.AddActor(outline)

    iren.Initialize()
    renWin.Render()
    iren.Start()
    
def createVolume(image):
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

def vtk2gmsh(polydata, filename='out.geo'):

    file = open(filename,'w')
    lc=4 # a parametrer!!

    nbnod = polydata.GetNumberOfPoints()
    nbelm = polydata.GetNumberOfCells()
    print('reading',nbnod,'nodes and',nbelm,'elements')

    file.write("lc=%d;\n" % lc)

    for i in range(0,nbnod):
        line=polydata.GetPoints().GetPoint(i)
        file.write("Point(%d)={%e,%e,%e,lc};\n" % (i+1, float(line[0]), float(line[1]), float(line[2])))

    edges={}
    nextno=1
    for i in range(0,nbelm):
        cell=polydata.GetCell(i)
        line=cell.GetPointIds()
        if cell.GetNumberOfPoints()!=3:
            print("bad elem (%d nodes)" % cell.GetNumberOfPoints())
            file.close()
            sys.exit()
        n1=cell.GetPointId(0)+1
        n2=cell.GetPointId(1)+1
        n3=cell.GetPointId(2)+1
        eds = [(n1,n2),(n2,n3),(n3,n1)]
        edno = [0, 0, 0]
        for j in range(0,3):
            edge=eds[j];
            sign=1
            edno[j]=0
            if edge[0]>edge[1]:
                sign=-1
                edge=(edge[1],edge[0])
            if edge in edges:
                edno[j]=edges[edge]*sign
            else:
                edno[j]=nextno*sign
                edges[edge]=nextno
                file.write("Line(%d) = {%d,%d};\n" %(nextno, edge[0], edge[1]))
                nextno=nextno+1
        file.write("Line Loop(%d) = {%d,%d,%d};\n" % (nextno, edno[0], edno[1], edno[2]))
        file.write("Plane Surface(%d) = {%d};\n" % (i+1, nextno))
        nextno=nextno+1

    file.write("Coherence;\n")
    file.write("Surface Loop(%d) = {" % (nbelm+1))
    for i in range(0,nbelm):
        file.write("%d" % (i+1))
        if i==nbelm-1:
            file.write("};\n")
        else:
            file.write(",")
    file.write("Volume(1) = {%d};\n" % (nbelm+1))
    file.write("Physical Volume(1) = {1};\n")
    file.write("Mesh.CharacteristicLengthFactor = %d;\n" % lc)
    file.close()
    