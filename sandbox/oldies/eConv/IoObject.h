#ifndef IOOBJECT_H
#define IOOBJECT_H

#include <stdio.h>
#include <string>
#include "IoError.h"

/**
 * @brief Basic I/O functions
 *
 * @author Romain BOMAN
 * @since  September 2003
 */

class IoObject
{
protected:
    bool readFlagOk(int flag) const;
    void openFile_R(FILE**file, const std::string &fileName) const;
    void openFile_W(FILE**file, const std::string &fileName) const;
    void closeFile(FILE**file) const;
};

#endif
