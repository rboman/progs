#ifndef MATLABTOOLEXPORTER_H
#define MATLABTOOLEXPORTER_H

#include "ToolExporter.h"

/**
 *  @brief Exports a M file that defines the Tool in Matlab.
 */

class MatlabToolExporter : public ToolExporter
{
public:
    MatlabToolExporter(Tool &_matrix);

private:
    virtual std::string getName() const;
    virtual std::string getFileExtension() const;
    virtual void writeHeader();
    virtual void writePoints();
    virtual void writeFooter();
};

#endif
