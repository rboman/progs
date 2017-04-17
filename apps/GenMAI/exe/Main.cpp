#include "Global.h"

extern void genMesh();
extern void genTool();

/**
 * @brief console application
 */

int main(int argc, char **argv) 
{
    genMesh();
    genTool();
    return 0;
}
