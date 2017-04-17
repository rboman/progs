#ifndef OOFELIETOOLEXPORTER_H
#define OOFELIETOOLEXPORTER_H

#include "ToolExporter.h"

/**
 *  @brief Exports a E file that defines the Tool for Oofelie.
 */

class OofelieToolExporter : public ToolExporter
{
public:
    OofelieToolExporter(Tool &_matrix);

private:
    virtual std::string getName() const;
    virtual std::string getFileExtension() const;
    virtual void writeHeader();
    virtual void writePoints();
    virtual void writeCurves();
    virtual void writeContours();
    virtual void writeFooter();
};

#endif
