# -*- coding: utf-8 -*-


import vtk
colors = vtk.vtkNamedColors()

class MyPickAreaCallBack:

    def __init__(self):

        pass

    def __call__(self, object, eid):   

        print('pick')
        print(f'\tobject = {object}') # object = vtkAreaPicker
        print(f'\teid = {eid}') # eid = EndPickEvent


