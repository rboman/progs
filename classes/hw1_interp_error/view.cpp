#include "view.h"
#include <cassert>
#include <gmsh.h>

void view(const std::vector<std::size_t> &elems2D,
          const std::vector<std::vector<double>> &nodalF,
          std::string const &vname)
{
    int viewtag = gmsh::view::add(vname);
    std::vector<std::string> names;
    gmsh::model::list(names); // we assume one model name
    assert(names.size() == 1);
    int step = 0;
    double time = 0.0;
    gmsh::view::addModelData(viewtag, step, names[0], "ElementNodeData",
                             elems2D, nodalF, time, 1); // the last ,1 is important!
    // gmsh::view::write(viewtag, "results.msh");

    // set display options & view results
    gmsh::option::setNumber("View[" + std::to_string(viewtag - 1) + "].IntervalsType", 3);
    // improve display of high-order elements...
    gmsh::option::setNumber("View[" + std::to_string(viewtag - 1) + "].AdaptVisualizationGrid", 1);
    gmsh::option::setNumber("View[" + std::to_string(viewtag - 1) + "].MaxRecursionLevel", 3);
    gmsh::option::setNumber("View[" + std::to_string(viewtag - 1) + "].TargetError", -0.001);
}
