#include "groups.h"
#include <gmsh.h>

void fillGroups(PhysicalGroups &groups)
{

    gmsh::vectorpair dimTags;
    gmsh::model::getPhysicalGroups(dimTags);
    for (size_t i = 0; i < dimTags.size(); ++i)
    {
        int dim = dimTags[i].first;
        int tag = dimTags[i].second;
        std::string name;
        gmsh::model::getPhysicalName(dim, tag, name);
        groups.dimTags[name] = {dim, tag};
    }
}
