#! /usr/bin/env python3
# -*- coding: utf-8; -*-

# meshing with vtk, isosurf, tetgen

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
