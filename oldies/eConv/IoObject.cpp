//
// $Id$
//

#include "IoObject.h"
#include <iostream>

bool 
IoObject::readFlagOk(int flag) const
{
    return (flag!=0 && flag!=EOF);
}

void
IoObject::openFile_R(FILE**file, const std::string &fileName) const
{
    *file = fopen(fileName.c_str(), "rt");
    if(!*file) throw IoError(fileName, "Cannot open file!");
}

void
IoObject::openFile_W(FILE**file, const std::string &fileName) const
{
    *file = fopen(fileName.c_str(), "wt");
    if(!*file) throw IoError(fileName, "Cannot open file!");
}

void 
IoObject::closeFile(FILE**file) const
{
    if(*file) 
        fclose(*file);
    else
        throw IoError("", "File is already closed!");
    *file=NULL;
}

