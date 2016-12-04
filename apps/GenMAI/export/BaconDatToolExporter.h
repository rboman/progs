//
// $Id$
//

#ifndef BACONDATTOOLEXPORTER_H
#define BACONDATTOOLEXPORTER_H

#include "ToolExporter.h"

/**
 * @brief Exports a DAT file for Bacon that can be used for displaying the Tool on the screen
 */

class BaconDatToolExporter : public ToolExporter
{
public:
    BaconDatToolExporter(Tool &_matrix);

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
