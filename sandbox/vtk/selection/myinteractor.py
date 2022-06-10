# -*- coding: utf-8 -*-


import vtk
colors = vtk.vtkNamedColors()

class MyInteractorStyle(vtk.vtkInteractorStyleTrackballCamera):

    def __init__(self, parent=None):
        """register to event listening
        """
        self.AddObserver("LeftButtonPressEvent", self.leftButtonPressEvent)

        self.selection = None
        self.selected_mapper = vtk.vtkDataSetMapper() 
        self.selected_actor = vtk.vtkActor()
        self.dataset = None

    def select_one(self):

        # get the mouse click position
        clickPos = self.GetInteractor().GetEventPosition()

        # crete a picker and pick at that position
        picker = vtk.vtkCellPicker()
        picker.Pick(clickPos[0], clickPos[1], 0, self.GetDefaultRenderer())

        print("pick")
        print(f"\tcell id = {picker.GetCellId()}")
        print(f"\t3D pick position = {picker.GetPickPosition()}")
        print(f"\t2D mouse position = {picker.GetSelectionPoint()[:2]}")
        
        # the picking could be empty
        # in that case, we leave the routine
        if picker.GetDataSet():
            print(f"\tdataset = {picker.GetDataSet().GetClassName()}")
        else:
            print(f"\tdataset = None")
            return

        # no cell has been picked => quit
        if picker.GetCellId()==-1:
            return

        # cell type - we can pick triangles, but also tetras 
        cell_type = picker.GetDataSet().GetCellType( picker.GetCellId() )
        print(f"\tcell type = { vtk.vtkCellTypes.GetClassNameFromTypeId( cell_type )}")
        if(cell_type != vtk.VTK_TRIANGLE ):
            print("\tWRONG CELL TYPE")
            return

        # we can pick the wrong ugrid (the red one)
        # we store the right one at the first successful picking
        if self.dataset == None:
            self.dataset = picker.GetDataSet()
        if picker.GetDataSet() != self.dataset:
            print(f"\tPICKED WRONG DATASET!")
            return
        
        # -- cree un "vtkSelectionNode" (données de selection + type de selection)
        ids = vtk.vtkIdTypeArray()
        ids.SetNumberOfComponents(1)
        ids.InsertNextValue(picker.GetCellId())

        selectionNode = vtk.vtkSelectionNode()
        selectionNode.SetFieldType(vtk.vtkSelectionNode.CELL)
        #  CELL,POINT,FIELD,VERTEX,EDGE,ROW
        selectionNode.SetContentType(vtk.vtkSelectionNode.INDICES)
        # SELECTIONS,GLOBALIDS,PEDIGREEIDS,VALUES,INDICES,FRUSTUM,
        #   LOCATIONS,THRESHOLDS,BLOCKS,QUERY     
        selectionNode.SetSelectionList(ids)

        # -- cree une "vtkSelection" (la sélection en elle-même)
        # c'est un ensemble de "noeuds de selection"
        if not self.selection:
            self.selection = vtk.vtkSelection()
            self.selection.AddNode(selectionNode)
        else:
            self.selection.Union(selectionNode)

        print( f"\tThere are {self.selection.GetNumberOfNodes()} 'selection nodes'.")


        # -- DISPLAY: cree une "vtkExtractSelection"
        extractSelection = vtk.vtkExtractSelection()
        extractSelection.SetInputData(0, picker.GetDataSet())
        # extractSelection.SetInputConnection(0, filt.GetOutputPort()) # cas d'un filtre
        extractSelection.SetInputData(1, self.selection)
        extractSelection.Update()

        # build a ugrid for display
        selected = vtk.vtkUnstructuredGrid()
        selected.ShallowCopy(extractSelection.GetOutput())

        print( f"\tThere are {selected.GetNumberOfPoints()} points in the selection.")
        print( f"\tThere are {selected.GetNumberOfCells()} cells in the selection.")

        self.selected_mapper.SetInputData(selected)
        self.selected_actor.SetMapper(self.selected_mapper)
        self.selected_actor.GetProperty().EdgeVisibilityOn()
        self.selected_actor.GetProperty().SetColor( colors.GetColor3d('red') )

        self.selected_actor.GetProperty().SetLineWidth(3)

        self.GetDefaultRenderer().AddActor(self.selected_actor) # global - n'est pas ajouté si il l'a deja été
        print(f'nb of actors = {self.GetDefaultRenderer().GetActors().GetNumberOfItems()}')


    def leftButtonPressEvent(self, obj, event):
        """custom event
        """
        self.select_one()
        self.OnLeftButtonDown() # calls vtk.vtkInteractorStyleTrackballCamera
