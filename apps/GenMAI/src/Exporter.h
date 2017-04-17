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
    void setBaseFileName(std::string &_name);
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

