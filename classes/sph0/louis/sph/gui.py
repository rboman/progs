#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# converts .res files to Paraview (VTK)
#
# - This script is run at the end of each test.
# - You may want to run it from the workspace/test* folders
#   if you want to rebuild the vtk files or in case of errors.
#
# Note on efficiency:
#   Execution is rather slow for large meshes and long simulations.
#   => Could be rewritten in C++.
#
#   tests_waterdrop4: 21'34" (sequential)
#                      2'22" (parallel - 20 threads) - speedup x9.1

import os, glob, vtk, multiprocessing

class ToParaview:
    def __init__(self, verb=False):
        self.verb = verb

    def convertall(self, pattern='res*.res'):
        if self.verb:
            print("converting grid to VTK")
        self.convertGrid()

        if self.verb:
            print("converting %s to VTK" % pattern)
        pool = multiprocessing.Pool()
        for result in pool.map(self.convertParts, glob.glob(pattern)):
            pass
        # for f in glob.glob(pattern):
        #     self.convertParts(f)

    def convertGrid(self, fname='grid.out'):
        """build a structured grid from the grid.out file
        """
        outname = fname.replace('.out', '.vts')

        if os.path.exists(outname):
            return

        if self.verb:
            print('converting', fname)

        # read file produced by the SPH code
        #   contains 2 scalars: nx and dx
        file = open(fname)
        line = file.readline()
        nx, dx = line.strip().split()
        nx = int(nx)
        dx = float(dx)
        file.close()

        # build a VTK structured grid
        grid = vtk.vtkStructuredGrid()
        points = vtk.vtkPoints()
        grid.SetPoints(points)

        for k in range(nx + 1):
            for j in range(nx + 1):
                for i in range(nx + 1):
                    points.InsertNextPoint(i * dx, j * dx, k * dx)
        grid.SetDimensions(nx + 1, nx + 1, nx + 1)

        # save the grid to disk
        writer = vtk.vtkXMLStructuredGridWriter()
        compressor = vtk.vtkZLibDataCompressor()
        writer.SetCompressor(compressor)
        writer.SetDataModeToBinary()
        writer.SetInputData(grid)
        writer.SetFileName(outname)
        writer.Write()


    def convertParts(self, fname):
        """convert a .res file to a .vtu file
        """
        outname = fname.replace('.res', '.vtu')

        if os.path.exists(outname):
            return

        if self.verb:
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

        # save the unstructured grid to disk
        writer = vtk.vtkXMLUnstructuredGridWriter()
        compressor = vtk.vtkZLibDataCompressor()
        writer.SetCompressor(compressor)
        writer.SetDataModeToBinary()
        writer.SetInputData(ugrid)
        writer.SetFileName(outname)
        writer.Write()


if __name__ == "__main__":
    # by default: convert files in the current directory
    ToParaview(verb=False).convertall()
