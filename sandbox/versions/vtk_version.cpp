#include <vtkVersion.h>
#include <iostream>

int main()
{
    std::cout << vtkVersion::GetVTKVersion() << '\n';
    return 0;
}
