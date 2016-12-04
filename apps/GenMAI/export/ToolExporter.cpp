//
// $Id$
//

#include "Global.h"
#include "ToolExporter.h"

/**
 * @brief Constructor
 */

ToolExporter::ToolExporter(Tool &_matrix): Exporter(), matrix(_matrix)
{
    setBaseFileName( std::string("mymatrix") );
}

void
ToolExporter::writeBody()
{
    writePoints();
    writeCurves();
    writeContours();
}

/**
 * @brief Writes the points to the file
 */

void 
ToolExporter::writePoints() 
{
}

/**
 * @brief Writes the curves to the file
 */

void 
ToolExporter::writeCurves() 
{
}

/**
 * @brief Writes the contours to the file
 */

void 
ToolExporter::writeContours() 
{
}

