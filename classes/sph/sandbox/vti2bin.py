import vtk

reader = vtk.vtkXMLImageDataReader ()
reader.SetFileName('machoireInf.vti')
reader.Update()
img = reader.GetOutput()

print img
print img.GetSpacing()

print img.GetNumberOfCells()

#celldata = img.GetCellData()
celldata = img.GetPointData()
scalars = celldata.GetScalars()
print celldata

f = open('out.bin','wb')

vals = []
for i in xrange(img.GetNumberOfCells()):
    c = scalars.GetTuple(i)
    vals.append(c[0])

import struct
s = struct.pack('f'*len(vals), *vals)
f.write(s)
f.close()



