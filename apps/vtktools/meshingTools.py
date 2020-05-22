#! /usr/bin/env python
# -*- coding: utf-8; -*-
# $Id: meshingTools.py 1778 2013-07-19 10:06:35Z laurent $
# Vinciane d'Otreppe

# meshing with vtk, isosurf, tetgen

from __future__ import print_function
from __future__ import division
from builtins import input
from builtins import range
from builtins import object
from past.utils import old_div
import vtk
import math
import generalTools
import imagingTools


def callTetgen(poly, q=2.0, a=0, V=0, Y=0):
    """
    -q 	Quality mesh generation. A minimum radius-edge ratio may be specifyed (default 2.0).
    -a 	Applies a maximum tetrahedron volume constraint.
    -Y 	Suppresses the creation of Steiner points on the exterior boundary (useful when the mesh boundary must be preserved so that it conforms to some adjacent mesh)  
    -V  prints a quality report
    Other options are possible -> see Tetgen's user manual
    """
    fname = "tmp"
    generalTools.vtk2tetgen(poly, fname+".poly")
    if (Y and V and (not a)):
        cmd = "tetgen -pYVq%gA %s" % (q, fname)
    elif (Y and V and a):
        cmd = "tetgen -pYVq%ga%g %s" % (q, a, fname)
    elif (Y and (not V)) and (not a):
        cmd = "tetgen -pYVq%gA %s" % (q, fname)
    elif (Y and (not V) and a):
        cmd = "tetgen -pYq%ga%g %s" % (q, a, fname)
    elif ((not Y) and V and (not a)):
        cmd = "tetgen -pVq%g %s" % (q, fname)
    elif ((not Y) and (not V) and (not a)):
        cmd = "tetgen -pq%g %s" % (q, fname)
    else:
        cmd = "tetgen -pq%ga%g %s" % (q, a, fname)

    print(cmd)
    #import os
    # os.system(cmd)
    import subprocess
    subprocess.call(cmd, shell=True)
    ugrid = generalTools.tetgen2vtk(fname)
    return ugrid


def callTetgenMultipleRegions(poly, regionSeeds, regionVolmax=[], regionHoles=[], q=2.0, V=0, Y=0):
    """
    -q 	Quality mesh generation. A minimum radius-edge ratio may be specifyed (default 2.0).
    -a 	Applies a maximum tetrahedron volume constraint.
    -V  prints a quality report
    -A  activates multiple regions attributes
    Other options are possible -> see Tetgen's user manual
    """
    fname = "tmp"
    arrayName = False
    if (regionVolmax == []):
        volmax = []
        for i in range(len(regionSeeds)):
            volmax.append(100)
        generalTools.vtk2tetgen(poly, fname+".poly",
                                arrayName, regionSeeds, volmax, regionHoles)
    else:
        generalTools.vtk2tetgen(
            poly, fname+".poly", arrayName, regionSeeds, regionVolmax, regionHoles)
    if (V and Y and regionVolmax == []):
        cmd = "tetgen -pYVq%gA %s" % (q, fname)
    elif (V and Y):
        cmd = "tetgen -pYVq%gaA %s" % (q, fname)
    elif (Y and regionVolmax == []):
        cmd = "tetgen -pYq%gA %s" % (q, fname)
    elif (Y):
        cmd = "tetgen -pYq%gaA %s" % (q, fname)
    elif (V and (not Y) and regionVolmax == []):
        cmd = "tetgen -pVq%gA %s" % (q, fname)
    elif (V and (not Y)):
        cmd = "tetgen -pVq%gaA %s" % (q, fname)
    elif ((not Y) and regionVolmax == []):
        cmd = "tetgen -pq%gA %s" % (q, fname)
    else:
        cmd = "tetgen -pq%gaA %s" % (q, fname)

    print(cmd)

    print("tetgen start")
    #import os
    # os.system(cmd)
    import subprocess
    subprocess.call(cmd, shell=True)

    print("tetgen done")

    ugrid = generalTools.tetgen2vtk(fname)
    return ugrid


def callTetgenMultipleRegionsFromGenisoMesh(genisoMesh, regionLabels, a, q=1.41, V=0):
    """
    -q 	Quality mesh generation. A minimum radius-edge ratio may be specifyed (default 2.0).
    -a 	Applies a maximum tetrahedron volume constraint.
    -V  prints a quality report
    -A  activates multiple regions attributes
    """
    print('TETGEN')
    fname = "tmp"
    marker = 1
    generalTools.vtk2tetgenMultipleRegionsFromGeniso(
        genisoMesh, regionLabels, a, fname+".poly")
    cmd = "tetgen -pVq%gaA %s" % (q, fname)
    print(cmd)
    #import os
    # os.system(cmd)

    import subprocess
    subprocess.call(cmd, shell=True)

    ugrid = generalTools.tetgen2vtk(fname)
    return ugrid


def callTetgenMultipleRegionsFromFile(fname="tmp", q=1.41, V=0):
    """
    -q 	Quality mesh generation. A minimum radius-edge ratio may be specifyed (default 2.0).
    -a 	Applies a maximum tetrahedron volume constraint.
    -V  prints a quality report
    -A  activates multiple regions attributes
    """
    if (V):
        cmd = "tetgen -pVq%gaA %s" % (q, fname)
    else:
        cmd = "tetgen -pq%gaA %s" % (q, fname)
    print(cmd)
    #import os
    # os.system(cmd)

    import subprocess
    subprocess.call(cmd, shell=True)
    ugrid = generalTools.tetgen2vtk(fname)
    return ugrid


def mergeDuplicateNodes(poly, tol=0.000001):
    cleaner = vtk.vtkCleanPolyData()
    cleaner.SetInput(poly)
    cleaner.SetTolerance(tol)
    cleaner.Update()
    return cleaner.GetOutput()


def extractLargestPoly(poly):
    connectivity = vtk.vtkPolyDataConnectivityFilter()
    connectivity.SetInput(poly)
    connectivity.SetExtractionModeToLargestRegion()
    connectivity.Update()
    return connectivity.GetOutput()


def extractNearestPoly(poly, x, y, z):
    # mergeDuplicateNodes may be needed before applying this filter
    connectivity = vtk.vtkPolyDataConnectivityFilter()
    connectivity.SetInput(poly)
    connectivity.SetExtractionModeToClosestPointRegion()
    connectivity.SetClosestPoint(x, y, z)
    connectivity.Update()
    return connectivity.GetOutput()


def extractSurfacesFromCompatiblePoly(poly):
    # Extract the set of polydata consituting the compatible polydata generated by geniso
    # Poly must contain an intArray called "node nearest volume" in poly.GetPointData()
    print(poly.GetPointData().GetNumberOfArrays())
    if poly.GetPointData().GetNumberOfArrays() == 0:
        print("ERROR in extractSurfacesFromCompatiblePoly: no array in Poly Pointdata ")
        input()

    intArray = poly.GetPointData().GetArray(0)
    print("intArray", intArray)

    nbnod = poly.GetNumberOfPoints()
    nbelm = poly.GetNumberOfCells()

    nodeSurfId = []
    newPolyNodes = []
    newPolyCells = []

    for i in range(0, nbnod):
        nodeMarker = intArray.GetValue(i)
        if nodeMarker not in nodeSurfId:
            nodeSurfId.append(nodeMarker)
            newPolyNodes.append(vtk.vtkPoints())
            newPolyCells.append(vtk.vtkCellArray())
        for j in enumerate(nodeSurfId):
            if j[1] == nodeMarker:
                newPolyNodes[j[0]].InsertPoint(
                    i, poly.GetPoints().GetPoint(i)[0], poly.GetPoints().GetPoint(i)[1], poly.GetPoints().GetPoint(i)[2])
                break

    for c in range(0, nbelm):
        cell = poly.GetCell(c)
        if cell.GetNumberOfPoints() != 3:
            print("Error in extractSurfacesFromCompatiblePoly: cell.GetNumberOfPoints != 3")
            input()

        for j in range(3):
            for i in enumerate(nodeSurfId):
                if i[1] == intArray.GetValue(cell.GetPointId(j)):
                    newPolyCells[i[0]].InsertNextCell(3)
                    newPolyCells[i[0]].InsertCellPoint(cell.GetPointId(0))
                    newPolyCells[i[0]].InsertCellPoint(cell.GetPointId(1))
                    newPolyCells[i[0]].InsertCellPoint(cell.GetPointId(2))
                    break
            for jj in range(3):
                if jj != j:
                    if intArray.GetValue(cell.GetPointId(jj)) != intArray.GetValue(cell.GetPointId(j)):
                        for i in enumerate(nodeSurfId):
                            if i[1] == intArray.GetValue(cell.GetPointId(j)):
                                id = cell.GetPointId(jj)
                                newPolyNodes[i[0]].InsertPoint(
                                    id, poly.GetPoints().GetPoint(id))
                                break

    vtkPolys = []
    for i in range(len(nodeSurfId)):
        vtkPoly = vtk.vtkPolyData()
        vtkPoly.SetPoints(newPolyNodes[i])
        vtkPoly.SetPolys(newPolyCells[i])
        vtkPoly = mergeDuplicateNodes(vtkPoly, 0.0001)
        vtkPolys.append(vtkPoly)

    return vtkPolys


def extractSurfacesAndInterfaceFromCompatiblePoly(poly):
    # Extract the set of polydata consituting the compatible polydata generated by geniso
    # Poly must contain an intArray called "node nearest volume" in poly.GetPointData()
    if poly.GetPointData().GetNumberOfArrays() == 0:
        print("ERROR in extractSurfacesFromCompatiblePoly: no array in Poly Pointdata ")
        input()

    intArray = poly.GetPointData().GetArray(0)

    nbnod = poly.GetNumberOfPoints()
    nbelm = poly.GetNumberOfCells()

    nodeSurfId = []
    newPolyNodes = []
    newPolyCells = []

    for i in range(0, nbnod):
        nodeMarker = intArray.GetValue(i)
        if nodeMarker not in nodeSurfId:
            nodeSurfId.append(nodeMarker)
            newPolyNodes.append(vtk.vtkPoints())
            newPolyCells.append(vtk.vtkCellArray())
        for j in enumerate(nodeSurfId):
            if j[1] == nodeMarker:
                newPolyNodes[j[0]].InsertPoint(
                    i, poly.GetPoints().GetPoint(i)[0], poly.GetPoints().GetPoint(i)[1], poly.GetPoints().GetPoint(i)[2])
                break

    newPolyNodes.append(vtk.vtkPoints())
    newPolyCells.append(vtk.vtkCellArray())

    for c in range(0, nbelm):
        cell = poly.GetCell(c)
        if cell.GetNumberOfPoints() != 3:
            print("Error in extractSurfacesFromCompatiblePoly: cell.GetNumberOfPoints != 3")
            input()
        marker0 = intArray.GetValue(cell.GetPointId(0))
        marker1 = intArray.GetValue(cell.GetPointId(1))
        marker2 = intArray.GetValue(cell.GetPointId(2))
        if(marker0 == marker1 and marker2 == marker1):
            for i in enumerate(nodeSurfId):
                if i[1] == intArray.GetValue(cell.GetPointId(0)):
                    newPolyCells[i[0]].InsertNextCell(3)
                    newPolyCells[i[0]].InsertCellPoint(cell.GetPointId(0))
                    newPolyCells[i[0]].InsertCellPoint(cell.GetPointId(1))
                    newPolyCells[i[0]].InsertCellPoint(cell.GetPointId(2))
                    break
        else:
            newPolyCells[len(newPolyCells)-1].InsertNextCell(3)
            for j in range(3):
                newPolyCells[len(newPolyCells) -
                             1].InsertCellPoint(cell.GetPointId(j))
                newPolyNodes[len(newPolyCells)-1].InsertPoint(cell.GetPointId(j),
                                                              poly.GetPoints().GetPoint(cell.GetPointId(j)))

    vtkPolys = []
    for i in range(len(newPolyNodes)):
        vtkPoly = vtk.vtkPolyData()
        vtkPoly.SetPoints(newPolyNodes[i])
        vtkPoly.SetPolys(newPolyCells[i])
        vtkPolys.append(vtkPoly)

    return vtkPolys


def extractClosedSurfacesFromPoly(poly):
    # Extract the set of polydata consituting the compatible polydata generated by geniso
    # Poly must contain an intArray called "node surface markers" in poly.GetPointData()

    if poly.GetPointData().GetNumberOfArrays() == 0:
        print("ERROR in extractSurfacesFromCompatiblePoly: no array in Poly Pointdata ")
        input()

    intArray = poly.GetPointData().GetArray(0)

    nbnod = poly.GetNumberOfPoints()
    nbelm = poly.GetNumberOfCells()

    nodeSurfId = []
    newPolyNodes = []
    newPolyCells = []

    for i in range(0, nbnod):
        nodeMarker = intArray.GetValue(i)
        if nodeMarker not in nodeSurfId:
            nodeSurfId.append(nodeMarker)
            newPolyNodes.append(vtk.vtkPoints())
            newPolyCells.append(vtk.vtkCellArray())

    for i in range(0, nbnod):
        nodeMarker = intArray.GetValue(i)
        for j0, j1 in enumerate(nodeSurfId):
            if nodeMarker != j1:
                newPolyNodes[j0].InsertPoint(
                    i, poly.GetPoints().GetPoint(i)[0], poly.GetPoints().GetPoint(i)[1], poly.GetPoints().GetPoint(i)[2])

    for j0, j1 in enumerate(nodeSurfId):
        print(j1)

    for c in range(0, nbelm):
        cell = poly.GetCell(c)
        if cell.GetNumberOfPoints() != 3:
            print("Error in extractSurfacesFromCompatiblePoly: cell.GetNumberOfPoints != 3")
            input()
        marker0 = intArray.GetValue(cell.GetPointId(0))
        marker1 = intArray.GetValue(cell.GetPointId(1))
        marker2 = intArray.GetValue(cell.GetPointId(2))
        for j0, j1 in enumerate(nodeSurfId):
            if(marker0 != j1 and marker1 != j1 and marker2 != j1):
                newPolyCells[j0].InsertNextCell(3)
                newPolyCells[j0].InsertCellPoint(cell.GetPointId(0))
                newPolyCells[j0].InsertCellPoint(cell.GetPointId(1))
                newPolyCells[j0].InsertCellPoint(cell.GetPointId(2))

    vtkPolys = []
    for i in range(len(newPolyNodes)):
        vtkPoly = vtk.vtkPolyData()
        vtkPoly.SetPoints(newPolyNodes[i])
        vtkPoly.SetPolys(newPolyCells[i])
        vtkPolys.append(vtkPoly)

    return vtkPolys


def smoothPolyData(poly, relax=0.1):
    # mergeDuplicateNodes may be needed before applying this filter
    smoother = vtk.vtkSmoothPolyDataFilter()
    smoother.SetInput(poly)
    smoother.SetRelaxationFactor(relax)
    smoother.Update()
    return smoother.GetOutput()


def activeSurface(segmentedImage, inputPolydata):
    '''
    Ferrant's Active surface Algorithm: The surface mesh (inputPolydata) given by mesh is deformed to cling as best as possible to the region boundary defined in segmentedImage. 

    Inputs:
        segmentedImage = target Image
        inputPolydata = initial mesh
    '''

    import vtkLocalPython

    imagingTools.createEdgesDistanceMap(segmentedImage)

    # Active Surface Algorithm
    filtActiveSurface = vtkLocalPython.vtkActiveSurfacePolyDataFilter()  # CPP function
    filtActiveSurface.SetNumberOfIterations(100)  # also used: 3, 200
    filtActiveSurface.SetTimeStep(0.05)
    filtActiveSurface.SetSmoothWeight(1.0)  # also used: 0.01
    filtActiveSurface.SetImageWeight(0.1)
    filtActiveSurface.SetEnergyImage(energyMap)
    filtActiveSurface.SetTargetImage(segmentedImage)
    filtActiveSurface.SetBoundaryConditionDistance(0.0)
    filtActiveSurface.SetLabelOfTargetStructure(100)
    filtActiveSurface.SetInput(inputPolydata)
    filtActiveSurface.WriteTmpResultsOff()
    filtActiveSurface.Update()

    return filtActiveSurface.GetOutput()


def duplicatePoly(poly1):
    points = vtk.vtkPoints()
    points.SetNumberOfPoints(poly1.GetNumberOfPoints())
    points.DeepCopy(poly1.GetPoints())
    poly = vtk.vtkPolyData()
    poly.SetPoints(points)
    cells = vtk.vtkCellArray()
    cells.DeepCopy(poly1.GetPolys())
    poly.SetPolys(cells)
    poly.BuildLinks(0)
    return poly


def cutVolumeMeshAccordingToDistanceMap(volumeMesh, distMap):
    '''
    donne sous forme d'un polydata la coupe d'un maillage volumique (unstructured grid) avec une fonction implicite
    coupe se fait ou la fonction implicite = 0
    utilisation de la classe vtkCutterLara plutot que vtkCutter pour augmenter la precision des points du polydata resultat
    (voir vtkCutterLara) sinon, points d'intersection ne sont pas bien localise sur le maillage volumique
    reference: Verdict reference manual, Stimpson, 2007
    '''
    import vtkLocalPython
    implicitDataSet = vtkLocalPython.vtkImplicitDataSetLara()
    implicitDataSet.SetDataSet(distMap)
    cutter = vtkLocalPython.vtkCutterLara()
    cutter.SetInput(volumeMesh)
    cutter.SetCutFunction(implicitDataSet)
    cutter.Update()
    return cutter.GetOutput()


def vtkDecimatePro(poly, targetReduction):
    deci = vtk.vtkDecimatePro()
    deci.SetInput(poly)
    deci.SetTargetReduction(targetReduction)
    deci.PreserveTopologyOn()
    deci.SplittingOff()
    deci.PreSplitMeshOff()
    deci.BoundaryVertexDeletionOff()
    deci.SetDegree(25)
    deci.SetFeatureAngle(90)
    return deci.GetOutput()


def vtkQuadricDecimation(poly, targetReduction):
    deci = vtk.vtkQuadricDecimation()
    deci.SetInput(poly)
    deci.SetTargetReduction(targetReduction)
    return deci.GetOutput()


def translate(poly, dx, dy, dz):
    t1 = vtk.vtkTransform()
    t1.Translate(dx, dy, dz)
    tr = vtk.vtkTransformPolyDataFilter()
    tr.SetInput(poly)
    tr.SetTransform(t1)
    poly = tr.GetOutput()
    poly.Update()
    return poly


def computeTriangleArea(vtkCell):
    p0, p1, p2 = vtkCell.GetPoints().GetPoint(
        0), vtkCell.GetPoints().GetPoint(1), vtkCell.GetPoints().GetPoint(2)
    return vtkCell.TriangleArea(p0, p1, p2)


def computeTriangleQualityEdgeRatio(vtkCell):
    ''' 
    Returns edge ratio of the triangular cell.
    Edge Ratio is defined as q = Lmax / Lmin
    where Lmax = largest edge Lmin = smallest edge
    Acceptable range = [1,1.3]
    for equilateral unit triangle = 1
    reference: Verdict reference manual, Stimpson, 2007
    '''
    qual = vtk.vtkMeshQuality()
    return qual.TriangleEdgeRatio(vtkCell)


def computeTriangleQualityAspectRatio(vtkCell):
    ''' 
    Returns aspect ratio of the triangle.
    Aspect Ratio is defined as q = Lmax / ( 2 * sqrt(3) * r ) = Lmax*(L0+L1+L2) / (4*sqrt(3)*Area)
    where Lmax = largest edge and r = inradius ( r = 2*Area/(L0+L1+L2) )
    Acceptable range = [1,1.3]
    for equilateral unit triangle = 1
    reference: Verdict reference manual, Stimpson, 2007
    '''
    qual = vtk.vtkMeshQuality()
    return qual.TriangleAspectRatio(vtkCell)


def computeTriangleQualityRadiusRatio(vtkCell):
    ''' 
    Returns [ min, average, max, variance, number of cells ] of Radius Ratio of the polydata's triangles.
    Radius ratio is defined as q = ( R ) / ( 2r )
    where R = circumradius ( = L0*L1*L2 / ( 2*r*(L0+L1+L2) ) ) and r = inradius ( r = 2*Area/(L0+L1+L2) )
    Acceptable range = [1,1.3]
    for equilateral unit triangle = 1
    reference: Verdict reference manual, Stimpson, 2007
    '''
    qual = vtk.vtkMeshQuality()
    return qual.TriangleRadiusRatio(vtkCell)


def computeTriangleQualityAspectFrobenius(vtkCell):
    ''' 
    Returns [ min, average, max, variance, number of cells ] of Aspect Frobenius of the polydata's triangles.
    Aspect Frobenius is defined as q = ( ||L0||^2+||L1||^2+||L2||^2 ) / ( 4*Area*sqrt(3) )
    Acceptable range = [1,1.3]
    for equilateral unit triangle = 1
    reference: Verdict reference manual, Stimpson, 2007
    '''
    qual = vtk.vtkMeshQuality()
    return qual.TriangleAspectFrobenius(vtkCell)


def computeTriangleQualityMinAngle(vtkCell):
    '''
    Returns [ min, average, max, variance, number of cells ] of Minmum included Angleof the polydata's triangles.
    Acceptable range = [30�,60�]
    for equilateral unit triangle = 60�
    reference: Verdict reference manual, Stimpson, 2007
    '''
    qual = vtk.vtkMeshQuality()
    return qual.TriangleMinAngle(vtkCell)


def computeTriangleQualityFrey(vtkCell):
    '''
    Quality measure proposed by Frey
    '''
    fact = old_div(6,math.sqrt(3))
    p0, p1, p2 = vtkCell.GetPoints().GetPoint(
        0), vtkCell.GetPoints().GetPoint(1), vtkCell.GetPoints().GetPoint(2)
    l0 = math.sqrt((p1[0]-p0[0])*(p1[0]-p0[0])+(p1[1]-p0[1])
                   * (p1[1]-p0[1])+(p1[2]-p0[2])*(p1[2]-p0[2]))
    l1 = math.sqrt((p2[0]-p1[0])*(p2[0]-p1[0])+(p2[1]-p1[1])
                   * (p2[1]-p1[1])+(p2[2]-p1[2])*(p2[2]-p1[2]))
    l2 = math.sqrt((p0[0]-p2[0])*(p0[0]-p2[0])+(p0[1]-p2[1])
                   * (p0[1]-p2[1])+(p0[2]-p2[2])*(p0[2]-p2[2]))
    area = vtkCell.TriangleArea(p0, p1, p2)
    lmax = max(l0, l1, l2)
    q = 0.0
    try:
        r = old_div(2*area,(l0+l1+l2))
        q = old_div(fact*r, lmax)
    except:
        pass
    return q


def computeCenter(vtkCell):
    nb = vtkCell.GetNumberOfPoints()
    center1 = 0
    center2 = 0
    center3 = 0
    for i in range(nb):
        center1 = center1+vtkCell.GetPoints().GetPoint(i)[0]
        center2 = center2+vtkCell.GetPoints().GetPoint(i)[1]
        center3 = center3+vtkCell.GetPoints().GetPoint(i)[2]
    center = [old_div(center1,nb), old_div(center2,nb), old_div(center3,nb)]
    return center


def computeTriangleQualitySemenova(vtkCell):
    p0, p1, p2 = vtkCell.GetPoints().GetPoint(
        0), vtkCell.GetPoints().GetPoint(1), vtkCell.GetPoints().GetPoint(2)
    l0 = math.sqrt((p1[0]-p0[0])*(p1[0]-p0[0])+(p1[1]-p0[1])
                   * (p1[1]-p0[1])+(p1[2]-p0[2])*(p1[2]-p0[2]))
    l1 = math.sqrt((p2[0]-p1[0])*(p2[0]-p1[0])+(p2[1]-p1[1])
                   * (p2[1]-p1[1])+(p2[2]-p1[2])*(p2[2]-p1[2]))
    l2 = math.sqrt((p0[0]-p2[0])*(p0[0]-p2[0])+(p0[1]-p2[1])
                   * (p0[1]-p2[1])+(p0[2]-p2[2])*(p0[2]-p2[2]))
    area = vtkCell.TriangleArea(p0, p1, p2)
    q = 0.0
    try:
        q = old_div(4*math.sqrt(3)*area, (l0*l0 + l1*l1 + l2*l2))
    except:
        pass
    return q


def computeQualities(polydata):
    '''
    Compute several qualities for poly and save in poly.GetCellData().GetArray(i)
    '''
    import vtk

    size = polydata.GetNumberOfCells()
    cellArray0 = vtk.vtkDoubleArray()
    cellArray0.SetName("QualityEdgeRatio")
    cellArray0.SetNumberOfValues(size)
    for i in range(size):
        cell = polydata.GetCell(i)
        q = computeTriangleQualityEdgeRatio(cell)
        cellArray0.InsertValue(i, q)
    polydata.GetCellData().AddArray(cellArray0)

    cellArray1 = vtk.vtkDoubleArray()
    cellArray1.SetName("QualityAspectRatio")
    cellArray1.SetNumberOfValues(size)
    for i in range(size):
        cell = polydata.GetCell(i)
        q = computeTriangleQualityAspectRatio(cell)
        cellArray1.InsertValue(i, q)
    polydata.GetCellData().AddArray(cellArray1)

    cellArray2 = vtk.vtkDoubleArray()
    cellArray2.SetName("QualityRadiusRatio")
    cellArray2.SetNumberOfValues(size)
    for i in range(size):
        cell = polydata.GetCell(i)
        q = computeTriangleQualityRadiusRatio(cell)
        cellArray2.InsertValue(i, q)
    polydata.GetCellData().AddArray(cellArray2)

    cellArray3 = vtk.vtkDoubleArray()
    cellArray3.SetName("QualityAspectFrobenius")
    cellArray3.SetNumberOfValues(size)
    for i in range(size):
        cell = polydata.GetCell(i)
        q = computeTriangleQualityAspectFrobenius(cell)
        cellArray3.InsertValue(i, q)
    polydata.GetCellData().AddArray(cellArray3)

    cellArray4 = vtk.vtkDoubleArray()
    cellArray4.SetName("QualityMinAngle")
    cellArray4.SetNumberOfValues(size)
    for i in range(size):
        cell = polydata.GetCell(i)
        q = computeTriangleQualityMinAngle(cell)
        cellArray4.InsertValue(i, q)
    polydata.GetCellData().AddArray(cellArray4)

    size = polydata.GetNumberOfCells()
    cellArray5 = vtk.vtkDoubleArray()
    cellArray5.SetName("QualityFrey")
    cellArray5.SetNumberOfValues(size)
    for i in range(size):
        cell = polydata.GetCell(i)
        q = computeTriangleQualityFrey(cell)
        cellArray5.InsertValue(i, q)
    polydata.GetCellData().AddArray(cellArray5)

    size = polydata.GetNumberOfCells()
    cellArray6 = vtk.vtkDoubleArray()
    cellArray6.SetName("CellArea")
    cellArray6.SetNumberOfValues(size)
    for i in range(size):
        cell = polydata.GetCell(i)
        q = computeTriangleArea(cell)
        cellArray6.InsertValue(i, q)
    polydata.GetCellData().AddArray(cellArray6)


def computePolyQualityEdgeRatio(polydata):
    ''' 
    Returns [ min, average, max, variance, number of cells ] of edge ratio of the polydata's triangles.
    Edge Ratio is defined as q = Lmax / Lmin
    where Lmax = largest edge Lmin = smallest edge
    Acceptable range = [1,1.3]
    for equilateral unit triangle = 1
    reference: Verdict reference manual, Stimpson, 2007
    '''
    qual = vtk.vtkMeshQuality()
    qual.SetInput(polydata)
    qual.VolumeOn()
    qual.RatioOn()
    qual.SetTriangleQualityMeasureToAspectRatio()
    qual.Update()
    return [qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(0), qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(1),
            qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(2),
            qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(3),
            qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(4)]


def computePolyQualityAspectRatio(polydata):
    ''' 
    Returns [ min, average, max, variance, number of cells ] of aspect ratio of the polydata's triangles.
    Aspect Ratio is defined as q = Lmax / ( 2 * sqrt(3) * r ) = Lmax*(L0+L1+L2) / (4*sqrt(3)*Area)
    where Lmax = largest edge and r = inradius ( r = 2*Area/(L0+L1+L2) )
    Acceptable range = [1,1.3]
    for equilateral unit triangle = 1
    reference: Verdict reference manual, Stimpson, 2007
    '''
    qual = vtk.vtkMeshQuality()
    qual.SetInput(polydata)
    qual.VolumeOn()
    qual.RatioOn()
    qual.SetTriangleQualityMeasureToAspectRatio()
    qual.Update()
    return [qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(1), qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(2),
            0,
            qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(3),
            qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(4)]


def computePolyQualityRadiusRatio(polydata):
    ''' 
    Returns [ min, average, max, variance, number of cells ] of Radius Ratio of the polydata's triangles.
    Radius ratio is defined as q = ( R ) / ( 2r )
    where R = circumradius ( = L0*L1*L2 / ( 2*r*(L0+L1+L2) ) ) and r = inradius ( r = 2*Area/(L0+L1+L2) )
    Acceptable range = [1,1.3]
    for equilateral unit triangle = 1
    reference: Verdict reference manual, Stimpson, 2007
    '''
    qual = vtk.vtkMeshQuality()
    qual.SetInput(polydata)
    qual.VolumeOn()
    qual.RatioOn()
    qual.SetTriangleQualityMeasureToRadiusRatio()
    qual.Update()
    return [qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(0), qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(1),
            qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(2),
            qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(3),
            qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(4)]


def computePolyQualityAspectFrobenius(polydata):
    ''' 
    Returns [ min, average, max, variance, number of cells ] of Aspect Frobenius of the polydata's triangles.
    Aspect Frobenius is defined as q = ( ||L0||^2+||L1||^2+||L2||^2 ) / ( 4*Area*sqrt(3) )
    Acceptable range = [1,1.3]
    for equilateral unit triangle = 1
    reference: Verdict reference manual, Stimpson, 2007
    '''
    qual = vtk.vtkMeshQuality()
    qual.SetInput(polydata)
    qual.VolumeOn()
    qual.RatioOn()
    qual.SetTriangleQualityMeasureToAspectFrobenius()
    qual.Update()
    return [qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(0), qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(1),
            qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(2),
            qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(3),
            qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(4)]


def computePolyQualityMinAngle(polydata):
    '''
    Returns [ min, average, max, variance, number of cells ] of Minmum included Angleof the polydata's triangles.
    Acceptable range = [30�,60�]
    for equilateral unit triangle = 60�
    reference: Verdict reference manual, Stimpson, 2007
    '''
    qual = vtk.vtkMeshQuality()
    qual.SetInput(polydata)
    qual.VolumeOn()
    qual.RatioOn()
    qual.SetTriangleQualityMeasureToMinAngle()
    qual.Update()
    return [qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(0), qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(1),
            qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(2),
            qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(3),
            qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(4)]


def computePolyQualityFrey(polydata):
    '''
    Quality measure proposed by Frey
    '''
    size = int(polydata.GetNumberOfCells())
    if size == 0:
        return [0, 0, 0, 0, 0]
    fact = old_div(6,math.sqrt(3))
    average = 0
    maximum = 0
    minimum = 1
    qualHistogram = []
    for i in range(101):
        qualHistogram.append(0)
    for i in range(size):
        cell = polydata.GetCell(i)
        q = computeTriangleQualityFrey(cell)
        if (q > maximum):
            maximum = q
        if (q < minimum):
            minimum = q
        average += q
        qualHistogram[int(q*100)] += 1
    average = old_div(average, size)

    file = open('polyQualityFrey.txt', 'w')
    file.write("max %f\n" % (maximum))
    file.write("min %f\n" % (minimum))
    file.write("average %f\n" % (average))
    file.write("histogram")
    for i, j in enumerate(qualHistogram):
        file.write(" %d" % (j))
    file.write("\n")
    file.close

    return [minimum, average, maximum, 0, size]


def computePolyArea(polydata):
    size = int(polydata.GetNumberOfCells())
    if size == 0:
        return [0, 0, 0, 0, 0]
    average = 0
    maximum = 0
    minimum = 1000
    for i in range(size):
        cell = polydata.GetCell(i)
        q = computeTriangleArea(cell)
        if (q > maximum):
            maximum = q
        if (q < minimum):
            minimum = q
        average += q
    average = old_div(average, size)

    return [minimum, average, maximum, 0, size]


def computePolyEdgeLengths(polydata):
    size = int(polydata.GetNumberOfCells())
    if size == 0:
        return [0, 0, 0, 0, 0]
    average = 0
    maximum = 0
    minimum = 1000

    for i in range(size):
        cell = polydata.GetCell(i)
        p0, p1, p2 = cell.GetPoints().GetPoint(
            0), cell.GetPoints().GetPoint(1), cell.GetPoints().GetPoint(2)
        l0 = math.sqrt((p1[0]-p0[0])*(p1[0]-p0[0])+(p1[1]-p0[1])
                       * (p1[1]-p0[1])+(p1[2]-p0[2])*(p1[2]-p0[2]))
        l1 = math.sqrt((p2[0]-p1[0])*(p2[0]-p1[0])+(p2[1]-p1[1])
                       * (p2[1]-p1[1])+(p2[2]-p1[2])*(p2[2]-p1[2]))
        l2 = math.sqrt((p0[0]-p2[0])*(p0[0]-p2[0])+(p0[1]-p2[1])
                       * (p0[1]-p2[1])+(p0[2]-p2[2])*(p0[2]-p2[2]))
        lmax = max(l0, l1, l2)
        lmin = min(l0, l1, l2)
        if (lmax > maximum):
            maximum = lmax
        if (lmin < minimum):
            minimum = lmin
        average += (l0+l1+l2)/3.0

    average = old_div(average, size)

    return [minimum, average, maximum, 0, size]


def computeUgridQualityEdgeRatio(ugrid):
    '''
    reference: Verdict reference manual, Stimpson, 2007
    '''
    qual = vtk.vtkMeshQuality()
    qual.SetInput(ugrid)
    qual.VolumeOn()
    qual.RatioOn()
    qual.SetTetQualityMeasureToAspectRatio()
    qual.Update()
    return [qual.GetOutput().GetFieldData().GetArray("Mesh Triangle Quality").GetValue(0), qual.GetOutput().GetFieldData().GetArray("Mesh Tetrahedron Quality").GetValue(1),
            qual.GetOutput().GetFieldData().GetArray(
                "Mesh Tetrahedron Quality").GetValue(2),
            qual.GetOutput().GetFieldData().GetArray(
                "Mesh Tetrahedron Quality").GetValue(3),
            qual.GetOutput().GetFieldData().GetArray("Mesh Tetrahedron Quality").GetValue(4)]


def computeUgridQualityAspectRatio(ugrid):
    '''
    reference: Verdict reference manual, Stimpson, 2007
    '''
    qual = vtk.vtkMeshQuality()
    qual.SetInput(ugrid)
    qual.VolumeOn()
    qual.RatioOn()
    qual.SetTetQualityMeasureToAspectRatio()
    qual.Update()
    return [qual.GetOutput().GetFieldData().GetArray("Mesh Tetrahedron Quality").GetValue(0), qual.GetOutput().GetFieldData().GetArray("Mesh Tetrahedron Quality").GetValue(1),
            qual.GetOutput().GetFieldData().GetArray(
                "Mesh Tetrahedron Quality").GetValue(2),
            qual.GetOutput().GetFieldData().GetArray(
                "Mesh Tetrahedron Quality").GetValue(3),
            qual.GetOutput().GetFieldData().GetArray("Mesh Tetrahedron Quality").GetValue(4)]


def computeUgridQualityRadiusRatio(ugrid):
    '''
    reference: Verdict reference manual, Stimpson, 2007
    '''
    qual = vtk.vtkMeshQuality()
    qual.SetInput(ugrid)
    qual.VolumeOn()
    qual.RatioOn()
    qual.SetTetQualityMeasureToRadiusRatio()
    qual.Update()
    return [qual.GetOutput().GetFieldData().GetArray("Mesh Tetrahedron Quality").GetValue(0), qual.GetOutput().GetFieldData().GetArray("Mesh Tetrahedron Quality").GetValue(1),
            qual.GetOutput().GetFieldData().GetArray(
                "Mesh Tetrahedron Quality").GetValue(2),
            qual.GetOutput().GetFieldData().GetArray(
                "Mesh Tetrahedron Quality").GetValue(3),
            qual.GetOutput().GetFieldData().GetArray("Mesh Tetrahedron Quality").GetValue(4)]


def computeUgridQualityFrobeniusNorm(ugrid):
    '''
    reference: Verdict reference manual, Stimpson, 2007
    '''
    qual = vtk.vtkMeshQuality()
    qual.SetInput(ugrid)
    qual.VolumeOn()
    qual.RatioOn()
    qual.SetTetQualityMeasureToFrobeniusNorm()
    qual.Update()
    return [qual.GetOutput().GetFieldData().GetArray("Mesh Tetrahedron Quality").GetValue(0), qual.GetOutput().GetFieldData().GetArray("Mesh Tetrahedron Quality").GetValue(1),
            qual.GetOutput().GetFieldData().GetArray(
                "Mesh Tetrahedron Quality").GetValue(2),
            qual.GetOutput().GetFieldData().GetArray(
                "Mesh Tetrahedron Quality").GetValue(3),
            qual.GetOutput().GetFieldData().GetArray("Mesh Tetrahedron Quality").GetValue(4)]


def computeUgridQualityMinAngle(ugrid):
    '''
    reference: Verdict reference manual, Stimpson, 2007
    '''
    qual = vtk.vtkMeshQuality()
    qual.SetInput(ugrid)
    qual.VolumeOn()
    qual.RatioOn()
    qual.SetTetQualityMeasureToMinAngle()
    qual.Update()
    return [qual.GetOutput().GetFieldData().GetArray("Mesh Tetrahedron Quality").GetValue(0), qual.GetOutput().GetFieldData().GetArray("Mesh Tetrahedron Quality").GetValue(1),
            qual.GetOutput().GetFieldData().GetArray(
                "Mesh Tetrahedron Quality").GetValue(2),
            qual.GetOutput().GetFieldData().GetArray(
                "Mesh Tetrahedron Quality").GetValue(3),
            qual.GetOutput().GetFieldData().GetArray("Mesh Tetrahedron Quality").GetValue(4)]


def computeUgridQualityTetGen(ugrid):
    '''
    In TetGen, quality of a tetra is measured as Q = R / Lmin where R = circumradius and Lmin = smallest edge
    '''


def alignPolyToReferencePoly(poly, polyRef):
    '''
    For each point in poly, locate closest in reference poly and write new poly
    -- Find Closest Point non disponible en Python
    '''
    from math import sqrt

    # cell locator
    cellLocator = vtk.vtkCellLocator()
    cellLocator.SetDataSet(polyRef)
    cellLocator.BuildLocator()

    #
    clospoint = [0, 0, 0]

    poi = poly.GetPoints()  # vtkPoints
    polys = poly.GetPolys()  # vtkCellArray
    npoints = poly.GetNumberOfPoints()
    for i in range(0, npoints):
        # find closest point
        cellLocator.FindClosestPoint(
            poi.GetPoint(i), clospoint, cellId, subId, dist)
        # Modify the Surface: Replace the poi with new coordinates.
        poi.SetPoint(i, clospoint)


def sortContourPoints(contour):
    n = contour.GetNumberOfCells()
    pointa = contour.GetPoint(contour.GetCell(0).GetPointId(0))
    pointToSearch = contour.GetPoint(contour.GetCell(0).GetPointId(1))
    points = vtk.vtkPoints()
    points.InsertNextPoint(pointa)
    c = list(range(1, n+1))
    id = 2
    while id < n+1:
        for cid in c:
            cell = contour.GetCell(cid)
            pointa = contour.GetPoint(cell.GetPointId(0))
            pointb = contour.GetPoint(cell.GetPointId(1))
            if ((abs(pointa[0]-pointToSearch[0]) < 1.e-5) and (abs(pointa[1]-pointToSearch[1]) < 1.e-5) and (abs(pointa[2]-pointToSearch[2]) < 1.e-5)):
                points.InsertNextPoint(pointa)
                pointToSearch = pointb
                c.remove(cid)
                break
            elif ((abs(pointb[0]-pointToSearch[0]) < 1.e-5) and (abs(pointb[1]-pointToSearch[1]) < 1.e-5) and (abs(pointb[2]-pointToSearch[2]) < 1.e-5)):
                points.InsertNextPoint(pointb)
                pointToSearch = pointa
                c.remove(cid)
                break
        id = id+1
    return points


def decimateContourPoints(inpoints, res):
    if inpoints.GetNumberOfPoints() < 10:
        return inpoints

    outpoints = vtk.vtkPoints()
    outpoints.InsertNextPoint(inpoints.GetPoint(0))
    id = 1
    while id < inpoints.GetNumberOfPoints():
        outpoints.InsertNextPoint(inpoints.GetPoint(id))
        id += res

    return outpoints


def scalePolydata(poly, scaleX, scaleY, scaleZ):
    tr = vtk.vtkTransform()
    tr.Scale(scaleX, scaleY, scaleZ)
    f = vtk.vtkTransformPolyDataFilter()
    f.SetInput(poly)
    f.SetTransform(tr)
    f.Update()
    return f.GetOutput()


def appendPolydatas(poly1, poly2):
    appendF = vtk.vtkAppendPolyData()
    appendF.AddInput(poly1)
    appendF.AddInput(poly2)
    appendF.Update()
    return appendF.GetOutput()


class createTringulatedCylinder(object):
    """ creation d'un cylindre triangul� 
    (hauteur des triangles �gale au nombre de triangles sur le p�rim�tre - setResolution(nb))
    """

    def __init__(self):
        self.radius = 5.
        self.resolution = 8
        self.height = 10.
        self.center = (0., 0., 0.)
        self.orientation = (0, -1, 0)
        self.capping = False

    def setRadius(self, radius=5.):
        self.radius = radius

    def setResolution(self, resolution=8):
        self.resolution = resolution

    def setHeight(self, height=10.):
        self.height = height

    def setCenter(self, center=(0., 0., 0.)):
        self.center = center

    def setOrient(self, orientation=(0, -1, 0)):
        self.orientation = orientation

    def setCapping(self, capping=False):
        self.capping = capping

    def execute(self):
        # vtkCylinderSource() = cylindre ax� selon -Ey avec des facettes rectangularies dont la hauteur est celle du cylindre
        # pour avoir un cylindre avec des triangles dont la hauteur est �gale a la base, on cr�e en fait une s�rie de cylindres dont le centre est d�cal�...
        localHeight = old_div(2.*math.pi*self.radius,self.resolution)
        nbCyl = int(math.ceil(old_div(self.height,localHeight)))
        height0 = self.height-nbCyl*localHeight
        # orientation par defaut du cylindre vtkCylinderSource()
        defaultOrientation = (0, -1, 0)
        cylindre = vtk.vtkCylinderSource()
        cylindre.SetRadius(self.radius)
        cylindre.SetResolution(self.resolution)
        cylindre.SetHeight(height0)
        cylindre.SetCenter(self.center)
        cylindre.SetCapping(0)
        cylindre.Update()
        if self.orientation != defaultOrientation:  # on reoriente le cylindre si necessaire
            a = self.orientation[0]
            b = self.orientation[1]
            c = self.orientation[2]
            norm = math.sqrt(a*a+b*b+c*c)
            self.orientation = (
                old_div(self.orientation[0],norm), old_div(self.orientation[1],norm), old_div(self.orientation[2],norm))
            alpha = math.atan2(self.orientation[0], self.orientation[1])
            beta = math.atan2(self.orientation[2], math.sqrt(
                self.orientation[0]*self.orientation[0]+self.orientation[1]*self.orientation[1]))
            # Vinciane: Correction pre et post translate !
            poly1 = translate(cylindre.GetOutput(
            ), -1.*self.center[0], -1.*self.center[1], -1.*self.center[2])
            transform = vtk.vtkTransform()
            transform.RotateX(math.degrees(beta))
            transform.RotateZ(math.degrees(180.-alpha))
            transformFilter = vtk.vtkTransformPolyDataFilter()
            transformFilter.SetInput(poly1)
            transformFilter.SetTransform(transform)
            transformFilter.Update()
            poly2 = transformFilter.GetOutput()
            output = translate(
                poly2, self.center[0], self.center[1], self.center[2])
        else:
            output = cylindre.GetOutput()
        # par defaut vtkCylinderSource() est maill� quadrangles, donc on triangularise
        triangleFilter = vtk.vtkTriangleFilter()
        triangleFilter.SetInput(output)
        triangleFilter.Update()
        cylindreComplet = triangleFilter.GetOutput()
        for cyl in range(1, nbCyl):
            cylindre = vtk.vtkCylinderSource()
            cylindre.SetCenter(
                self.center[0], self.center[1]-cyl*localHeight, self.center[2])
            cylindre.SetHeight(localHeight)
            cylindre.SetRadius(self.radius)
            cylindre.SetResolution(self.resolution)
            cylindre.SetCapping(self.capping)
            cylindre.Update()
            if self.orientation != defaultOrientation:
                poly1 = translate(cylindre.GetOutput(
                ), -1.*self.center[0], -1.*self.center[1], -1.*self.center[2])
                transform = vtk.vtkTransform()
                transform.RotateX(math.degrees(beta))
                transform.RotateZ(math.degrees(180.-alpha))
                transformFilter = vtk.vtkTransformPolyDataFilter()
                transformFilter.SetInput(poly1)
                transformFilter.SetTransform(transform)
                transformFilter.Update()
                poly2 = transformFilter.GetOutput()
                output = translate(
                    poly2, self.center[0], self.center[1], self.center[2])
            else:
                output = cylindre.GetOutput()
            triangleFilter = vtk.vtkTriangleFilter()
            triangleFilter.SetInput(output)
            triangleFilter.Update()
            cylindreComplet = appendPolydatas(
                cylindreComplet, triangleFilter.GetOutput())
        return cylindreComplet


def getBooleanOperationOnPolys(poly1, poly2, operation='union'):
    if operation == 'union':
        opVTK = vtk.VTK_UNION
    elif operation == 'intersection':
        opVTK = vtk.VTK_INTERSECTION
    elif operation == 'difference':
        opVTK = vtk.VTK_DIFFERENCE
    else:
        raise Exception(
            "getBooleanOperationOnPolys: unknown boolean operation type (known types are: 'union', 'intersection', and 'difference')")
    boolFilter = vtk.vtkBooleanOperationPolyDataFilter()
    boolFilter.SetOperation(opVTK)
    boolFilter.SetInputConnection(0, poly1.GetProducerPort())
    boolFilter.SetInputConnection(1, poly2.GetProducerPort())
    boolFilter.Update()
    return boolFilter.GetOutput()
