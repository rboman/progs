// writeMSH.cpp - "writeMSH" function and related routines

#include "gmshio.h"

/*
    * version 2.2 / 4
    
        $NodeData
        numStringTags(ASCII int)
        stringTag(string) ...
        numRealTags(ASCII int)
        realTag(ASCII double) ...
        numIntegerTags(ASCII int)
        integerTag(ASCII int) ...
        nodeNumber(int) value(double) ...
        $EndNodeData
*/

// write a ".pos" file (postprocessing file of gmsh
//    solution is a map which links a std::vector of values to each node
//    if its size should be equal to 1, 3 or 9 (scalar, vector or tensor)

void writeMSH(const char *fileName, double time, int step,
              std::map<Node *, std::vector<double>> &solution)
{
    FILE *fp = fopen(fileName, "w");
    if (!fp)
    {
        printf("Error: cannot open file %s\n", fileName);
        exit(1);
    }

    if (solution.empty())
        errorAndQuit("empty solution!");

    std::map<Node *, std::vector<double>>::iterator it = solution.begin();

    int nbComp = (int)(it->second.size());

    double version = 2.2; // same as 4.0
    fprintf(fp, "$MeshFormat\n%f 0 8\n$EndMeshFormat\n", version);
    fprintf(fp, "$NodeData\n");
    fprintf(fp, "%d\n", 1);                               // nb of string tags
    fprintf(fp, "\"my solution\"\n");                     // the name of the view
    fprintf(fp, "%d\n", 1);                               // nb of real tags
    fprintf(fp, "%g\n", time);                            // the time value
    fprintf(fp, "%d\n", 3);                               // nb of integer tags
    fprintf(fp, "%d\n", step);                            // time step index
    fprintf(fp, "%d\n", nbComp);                          // nb of field component (1, 3 or 9)
    fprintf(fp, "%lu\n", (unsigned long)solution.size()); // nb of entities in the view
    for (it = solution.begin(); it != solution.end(); it++)
    {
        fprintf(fp, "%d", it->first->num);
        for (int i = 0; i < nbComp; i++)
            fprintf(fp, " %g", it->second[i]);
        fprintf(fp, "\n");
    }
    fprintf(fp, "$EndNodeData\n");
    fclose(fp);
}
