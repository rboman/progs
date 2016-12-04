//
// $Id$
//

#include "Global.h"
#include <iostream>
#include "Exporter.h"

/**
 * @brief Constructor
 */

Exporter::Exporter() 
{ 
    fich=NULL; 
}

/**
 * @brief Opens the file
 */

void
Exporter::openFile()
{
    std::string fname = getFileName();
    fich = fopen(fname.c_str(),"w+t");
}

/**
 * @brief Closes the file
 */

void
Exporter::closeFile()
{
    fclose(fich);
}

/**
 * @brief Returns the filename with the extension
 */

std::string 
Exporter::getFileName() const
{
    return fileName + getFileExtension();
}

/**
 * @brief Sets the filename
 */

void 
Exporter::setBaseFileName(std::string &_name)
{
    fileName = _name;
}

/**
 * @brief Gets the filename
 */

std::string 
Exporter::getBaseFileName() const
{
    return fileName;
}

/**
 * @brief Prints the name of the Exporter to stdout
 */

void
Exporter::printBeginComment()
{
    std::cout << getName().c_str() << "Exporter:" << std::endl;
    std::cout << "----------------" << std::endl;
}

/**
 * @brief Prints some comments to stdout
 */

void
Exporter::printEndComment()
{
    std::cout << "fichier " << getName().c_str() << " ("<< getFileName().c_str() <<") cree." << std::endl << std::endl;
}

/**
 * @brief Main member function - exports the data to the file
 */

void
Exporter::save()
{
    printBeginComment();

    openFile();
    writeHeader();
    writeBody();
    writeFooter();
    closeFile();

    printEndComment();
}

