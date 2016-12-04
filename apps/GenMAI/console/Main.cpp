//
// $Id$
//

#include "Global.h"

extern void GenMesh();
extern void GenTool();


/**
 * @brief Stupid main : console
 */

int main( int argc, char ** argv ) 
{
    GenMesh();
    GenTool();

    return 0;
}
