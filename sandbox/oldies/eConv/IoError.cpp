#include "IoError.h"
#include <iostream>

IoError::IoError(const std::string &_file, const std::string &_desc) : file(_file), desc(_desc)
{
}

void IoError::print() const
{
    std::cerr << "IoError exception! file=" << file.c_str() << " : " << desc.c_str() << std::endl;
}
