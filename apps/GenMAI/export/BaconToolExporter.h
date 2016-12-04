//
// $Id$
//

#ifndef BACONTOOLEXPORTER_H
#define BACONTOOLEXPORTER_H

#include "ToolExporter.h"

/**
 *  @brief Exports a DON file in order to use the Tool in Metafor FORTRAN 
 */

class BaconToolExporter : public ToolExporter
{
public:
    BaconToolExporter(Tool &_matrix);

private:
    virtual std::string getName() const;
    virtual std::string getFileExtension() const;
    virtual void writeHeader();
    virtual void writePoints();
    virtual void writeCurves();
    virtual void writeFooter();
};

#endif
