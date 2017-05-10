#include "mailsph.h"
#include <vtkSmartPointer.h>
#include <vtkUnstructuredGrid.h>
#include <vtkReflectionFilter.h>
#include <vtkExtractUnstructuredGrid.h>
#include <vtkIncrementalOctreePointLocator.h>


vtkSmartPointer<vtkUnstructuredGrid> reflect(vtkSmartPointer<vtkUnstructuredGrid> ugrid)
{

	
	auto rfilterX = vtkSmartPointer<vtkReflectionFilter>::New();
	rfilterX->SetInputData(ugrid);
	rfilterX->CopyInputOn();
	rfilterX->SetPlaneToXMax();

	auto rfilterY = vtkSmartPointer<vtkReflectionFilter>::New();
	rfilterY->SetInputConnection(rfilterX->GetOutputPort());
	rfilterY->CopyInputOn();
	rfilterY->SetPlaneToYMax();

	auto rfilterZ = vtkSmartPointer<vtkReflectionFilter>::New();
	rfilterZ->SetInputConnection(rfilterY->GetOutputPort());
	rfilterZ->CopyInputOn();
	rfilterZ->SetPlaneToZMax();

	auto tougrid = vtkSmartPointer<vtkExtractUnstructuredGrid>::New();
	tougrid->MergingOn();
	auto ptInserter = vtkSmartPointer<vtkIncrementalOctreePointLocator>::New();
    ptInserter->SetTolerance(0.001); // default tol is too low
	tougrid->SetLocator(ptInserter);
	tougrid->SetInputConnection(rfilterZ->GetOutputPort());
	tougrid->Update();
	vtkSmartPointer<vtkUnstructuredGrid> ugrid2 = tougrid->GetOutput();

    return ugrid2;	
}