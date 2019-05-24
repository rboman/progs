//   Copyright 2003-2017 Romain Boman
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

#include "Exporter.h"
#include <iostream>

Exporter::Exporter()
{
    fich = NULL;
}

/**
 * @brief Opens the file
 */

void Exporter::openFile()
{
    std::string fname = getFileName();
    fich = fopen(fname.c_str(), "w+t");
}

/**
 * @brief Closes the file
 */

void Exporter::closeFile()
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

void Exporter::setBaseFileName(std::string const &_name)
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

void Exporter::printBeginComment()
{
    std::cout << getName().c_str() << "Exporter:" << std::endl;
    std::cout << "----------------" << std::endl;
}

/**
 * @brief Prints some comments to stdout
 */

void Exporter::printEndComment()
{
    std::cout << "fichier " << getName().c_str() << " (" << getFileName().c_str() << ") cree." << std::endl
              << std::endl;
}

/**
 * @brief Main member function - exports the data to the file
 */

void Exporter::save()
{
    printBeginComment();

    openFile();
    writeHeader();
    writeBody();
    writeFooter();
    closeFile();

    printEndComment();
}
