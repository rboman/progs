#include "mailsph.h"
#include <vtkSmartPointer.h>
#include <vtkUnstructuredGrid.h>
#include <vtkHexahedron.h>
#include <vtkQuad.h>

void insertvtkcell(vtkUnstructuredGrid *ugrid, int id1, int id2, int id3, int id4, int id5, int id6, int id7, int id8)
{
    vtkSmartPointer<vtkHexahedron> cell = vtkSmartPointer<vtkHexahedron>::New();
    vtkIdList *ids = cell->GetPointIds();  
    ids->SetId( 0, id1);
    ids->SetId( 1, id2);
    ids->SetId( 2, id3);
    ids->SetId( 3, id4);
    ids->SetId( 4, id5);
    ids->SetId( 5, id6);
    ids->SetId( 6, id7);
    ids->SetId( 7, id8);
    ugrid->InsertNextCell(cell->GetCellType(), ids);
}

void insertvtkcell(vtkUnstructuredGrid *ugrid, int id1, int id2, int id3, int id4)
{
    vtkSmartPointer<vtkQuad> cell = vtkSmartPointer<vtkQuad>::New();
    vtkIdList *ids = cell->GetPointIds();  
    ids->SetId( 0, id1);
    ids->SetId( 1, id2);
    ids->SetId( 2, id3);
    ids->SetId( 3, id4);
    ugrid->InsertNextCell(cell->GetCellType(), ids);
}
