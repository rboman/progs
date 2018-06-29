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

#ifndef EXPORTER_H
#define EXPORTER_H

#include <string>

/**
 * @brief Mother class for exports.
 */

class Exporter
{
    std::string fileName;          ///< filename (without extension)
protected:
    FILE *fich;                    ///< C file structure from stdio

public:
    Exporter();

    void save();

    std::string getFileName() const;
    void setBaseFileName(std::string const &_name);
    std::string getBaseFileName() const;

protected:
    /// Gets the filename (without extension)
    virtual std::string getName() const = 0;
    /// Gets the file extension
    virtual std::string getFileExtension() const = 0;
    /// Writes the header to the file
    virtual void writeHeader() = 0;
    /// Writes the body to the file
    virtual void writeBody() = 0;
    /// Writes the footer to the file
    virtual void writeFooter() = 0;

private:
    void openFile();
    void closeFile();
    void printBeginComment();
    void printEndComment();
};

#endif

