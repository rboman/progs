#include "mailsph.h"
#include <vtkSmartPointer.h>
#include <vtkXMLUnstructuredGridWriter.h>
#include <vtkZLibDataCompressor.h>

void exportvtu(vtkUnstructuredGrid *ugrid, std::string const &fname)
{
    // export to vtu file
    auto writer = vtkSmartPointer<vtkXMLUnstructuredGridWriter>::New();
    auto compressor = vtkSmartPointer<vtkZLibDataCompressor>::New();
    writer->SetCompressor(compressor);
    writer->SetDataModeToBinary();
    writer->SetInputData(ugrid);
    writer->SetFileName(fname.c_str());
    writer->Write();
    std::cout << fname << " saved to disk.\n";
}

