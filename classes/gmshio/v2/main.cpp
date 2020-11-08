//
// This program shows how to use readMSH and writeMSH functions
//

#include "gmshio.h"

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        printf("Usage: %s file.msh\n", argv[0]);
        return 1;
    }

    // read the mesh from a given .msh file
    std::vector<Node *> nodes;
    std::vector<Element *> elements;
    readMSH(argv[1], nodes, elements);

    printf("Read %lu nodes and %lu elements\n", (unsigned long)nodes.size(),
           (unsigned long)elements.size());

    double xmin = nodes[0]->x;
    double ymin = nodes[0]->y;
    double zmin = nodes[0]->z;
    double xmax = xmin;
    double ymax = ymin;
    double zmax = zmin;

    for (unsigned int i = 0; i < nodes.size(); i++)
    {
        if (nodes[i]->x < xmin)
            xmin = nodes[i]->x;
        if (nodes[i]->x > xmax)
            xmax = nodes[i]->x;
        if (nodes[i]->y < ymin)
            ymin = nodes[i]->y;
        if (nodes[i]->y > ymax)
            ymax = nodes[i]->y;
        if (nodes[i]->z < zmin)
            zmin = nodes[i]->z;
        if (nodes[i]->z > zmax)
            zmax = nodes[i]->z;
    }

    // create a solution and write .pos files for several dummy time steps
    std::map<Node *, std::vector<double>> solution;

    int nstep = 10;
    for (int step = 0; step < nstep; step++)
    {
        double sx = xmin + (xmax - xmin) * ((double)step) / nstep;
        double sy = ymin + (ymax - ymin) * ((double)step) / nstep;
        double sz = zmin + (zmax - zmin) * ((double)step) / nstep;

        for (unsigned int i = 0; i < nodes.size(); i++)
        {
            Node *n = nodes[i];
            double v = sqrt((n->x - sx) * (n->x - sx) + (n->y - sy) * (n->y - sy) + (n->z - sz) * (n->z - sz));
            std::vector<double> val(1, v);
            solution[n] = val;
        }
        // file name
        char name[256];
        sprintf(name, "sol_%04d.pos", step);

        // create a file containing the solution
        double time = 0.25 * step;
        writeMSH(name, time, step, solution);
    }

    return 0;
}
