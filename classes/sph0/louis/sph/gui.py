#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# converts .res files to Paraview (VTK)

import glob
import vtk
version = vtk.vtkVersion().GetVTKMajorVersion()


class ToParaview:
    def __init__(self, verb=False):
        self.verb = verb

    def convertall(self, pattern='*MP*.res'):
        print("converting grid to VTK")
        self.convertGrid()
        print("converting %s to VTK" % pattern)
        for f in glob.glob(pattern):
            self.convertParts(f)

    def convertGrid(self, fname='grid.out'):
        if self.verb:
            print('converting', fname)

        # read file
        file = open(fname)
        line = file.readline()
        nx, dx = line.strip().split()
        nx = int(nx)
        dx = float(dx)
        # print 'nx=%d, dx=%f' % (nx,dx)
        file.close()

        # build a sgrid
        grid = vtk.vtkStructuredGrid()
        points = vtk.vtkPoints()
        grid.SetPoints(points)

        for k in range(nx + 1):
            for j in range(nx + 1):
                for i in range(nx + 1):
                    points.InsertNextPoint(i * dx, j * dx, k * dx)
        grid.SetDimensions(nx + 1, nx + 1, nx + 1)

        writer = vtk.vtkXMLStructuredGridWriter()
        compressor = vtk.vtkZLibDataCompressor()
        writer.SetCompressor(compressor)
        writer.SetDataModeToBinary()
        if version > 5:
            writer.SetInputData(grid)
        else:
            writer.SetInput(grid)
        writer.SetFileName(fname.replace('.out', '.vts'))
        writer.Write()

    def convertParts(self, fname):

        # if self.verb:
        print('converting', fname)
        file = open(fname)

        ugrid = vtk.vtkUnstructuredGrid()
        points = vtk.vtkPoints()
        ugrid.SetPoints(points)

        codes = {'p': (1, "Pressure"),
                 'rho': (1, "Mass density"),
                 'v': (3, "Velocity"),
                 'm': (1, "Mass"),
                 'c': (1, "Speed of sound"),
                 'h': (1, "Smoothing length"),
                 'mu': (1, "max(mu_ab)"),
                 'nv': (1, "Nb of neighbours")}
        results = {}
        for c in codes:
            scalars = vtk.vtkFloatArray()
            ncomp, fullname = codes[c]
            scalars.SetNumberOfComponents(ncomp)
            scalars.SetName(fullname)
            ugrid.GetPointData().AddArray(scalars)
            results[c] = scalars

        i = 0
        for line in file:
            # x,y,z, vx,vy,vz, m, p = map(float, line.strip().split())
            try:
                x, y, z, vx, vy, vz, rho, p, m, c, h, mu, nv = list(map(float, line.strip().split()))
            except:
                print("**ERROR while reading file %s!\n\tline=\"%s\"" % (fname, line))
                break
            points.InsertPoint(i, x, y, z)
            results['v'].InsertNextTuple3(vx, vy, vz)
            results['p'].InsertNextValue(p)
            results['rho'].InsertNextValue(rho)
            results['m'].InsertNextValue(m)
            results['c'].InsertNextValue(c)
            results['h'].InsertNextValue(h)
            results['mu'].InsertNextValue(mu)
            results['nv'].InsertNextValue(nv)
            i += 1
        file.close()

        ntot = i
        if self.verb:
            print("\t%d lines read. Converting grid to VTK..." % ntot)

        for i in range(ntot):
            vertex = vtk.vtkVertex()
            ids = vertex.GetPointIds()
            ids.SetId(0, i)
            ugrid.InsertNextCell(vertex.GetCellType(), ids)

        if self.verb:
            print("\t...", ugrid.GetNumberOfPoints(), 'points and',
                  ugrid.GetNumberOfCells(), 'cells converted')

        writer = vtk.vtkXMLUnstructuredGridWriter()
        compressor = vtk.vtkZLibDataCompressor()
        writer.SetCompressor(compressor)
        writer.SetDataModeToBinary()
        if version > 5:
            writer.SetInputData(ugrid)
        else:
            writer.SetInput(ugrid)
        writer.SetFileName(fname.replace('.res', '.vtu'))
        writer.Write()


if __name__ == "__main__":
    ToParaview(verb=True).convertall()
