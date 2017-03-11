#ifndef TOOLEXPORTER_H
#define TOOLEXPORTER_H

#include "Exporter.h"
class Tool;

/**
 *  @brief Mother class for Tool export.
 */

class ToolExporter : public Exporter
{
protected:
    const Tool &matrix;

public:
    ToolExporter(Tool &_matrix);

protected:
    virtual void writePoints();
    virtual void writeCurves();
    virtual void writeContours();

private:
    virtual void writeBody();
};

#endif
