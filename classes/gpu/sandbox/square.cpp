// this example uses gmsh-sdk to create a transfinite mesh
//

#include <vector>
#ifdef _MSC_VER
#include <gmsh.h_cwrap>
#else
#include <gmsh.h>
#endif
#include <stdio.h>
#include <iostream>

int main(int argc, char **argv)
{
    gmsh::initialize(argc, argv);
    gmsh::option::setNumber("General.Terminal", 1); // enables "gmsh::logger::write(...)"

    /*
    // lecture d'un fichier .msh =================
    if (argc < 2)
    {
        std::cout << "Usage: " << argv[0] << " file.msh [options]" << std::endl;
        return 0;
    }
    gmsh::open(argv[1]);                            // reads the msh file
                                                    // lecture d'un fichier .msh =================
    */

    /*
    // creer un maillage d'un carré (avec OCC)
    gmsh::model::add("my square");
    gmsh::model::occ::addRectangle(0, 0, 0, 1, 1);
    gmsh::model::occ::synchronize();
    gmsh::model::mesh::generate();
    */

    // creer un maillage avec Built-in CAD kernel (traduction de "square.geo")
    gmsh::model::add("my square");

    double Lx = 5.0;
    double Ly = 5.0;
    double lc = 1.0;
    int n = 10;
    // Point(1) = {0, 0, 0, 1.0};
    // Point(2) = {Lx, 0, 0, 1.0};
    // Point(3) = {Lx, Ly, 0, 1.0};
    // Point(4) = {0, Ly, 0, 1.0};
    gmsh::model::geo::addPoint(0, 0, 0, lc, 1); // x,y,z, lc, tag
    gmsh::model::geo::addPoint(Lx, 0, 0, lc, 2);
    gmsh::model::geo::addPoint(Lx, Ly, 0, lc, 3);
    gmsh::model::geo::addPoint(0, Ly, 0, lc, 4);

    // Line(1) = {1, 2};
    // Line(2) = {2, 3};
    // Line(3) = {3, 4};
    // Line(4) = {4, 1};
    gmsh::model::geo::addLine(1, 2, 1); // pt1, pt2, tag
    gmsh::model::geo::addLine(2, 3, 2);
    gmsh::model::geo::addLine(3, 4, 3);
    gmsh::model::geo::addLine(4, 1, 4);

    // Curve Loop(1) = {3, 4, 1, 2};
    gmsh::model::geo::addCurveLoop({3, 4, 1, 2}, 1); // {curve nos}, tag

    // Plane Surface(1) = {1};
    gmsh::model::geo::addPlaneSurface({1}, 1); // {loops nos}, tag

    // Transfinite Curve {4, 1, 2, 3} = n+1 Using Progression 1;
    gmsh::model::geo::mesh::setTransfiniteCurve(1, n + 1, "Progression", 1.0);
    gmsh::model::geo::mesh::setTransfiniteCurve(2, n + 1, "Progression", 1.0);
    gmsh::model::geo::mesh::setTransfiniteCurve(3, n + 1, "Progression", 1.0);
    gmsh::model::geo::mesh::setTransfiniteCurve(4, n + 1, "Progression", 1.0);

    // Transfinite Surface {1};
    gmsh::model::geo::mesh::setTransfiniteSurface(1, "Left", {1, 2, 3, 4});

    // Physical Curve("left") = {4};
    gmsh::model::addPhysicalGroup(1, {4}, 1);   // dim, {surf nos}, tag
    gmsh::model::setPhysicalName(1, 1, "left"); // dim, tag, "name"
    // Physical Curve("right") = {2};
    gmsh::model::addPhysicalGroup(1, {2}, 2);
    gmsh::model::setPhysicalName(1, 2, "right");
    // Physical Curve("top") = {3};
    gmsh::model::addPhysicalGroup(1, {3}, 3);
    gmsh::model::setPhysicalName(1, 3, "top");
    // Physical Curve("bottom") = {1};
    gmsh::model::addPhysicalGroup(1, {1}, 4);
    gmsh::model::setPhysicalName(1, 4, "bottom");

    // Physical Surface("domain") = {1};
    gmsh::model::addPhysicalGroup(2, {1}, 1);     // dim, {surf nos}, tag
    gmsh::model::setPhysicalName(2, 1, "domain"); // dim, tag, "name"

    // generate mesh
    gmsh::model::geo::synchronize();
    gmsh::model::mesh::generate();

    gmsh::model::mesh::setOrder(3);

    // ======================================================================
    // from explore.cpp

    // get all elementary entities in the model
    std::vector<std::pair<int, int>> entities;
    gmsh::model::getEntities(entities);

    for (unsigned int i = 0; i < entities.size(); i++)
    {
        // get the mesh nodes for each elementary entity
        std::vector<std::size_t> nodeTags;
        std::vector<double> nodeCoords, nodeParams;
        int dim = entities[i].first, tag = entities[i].second;
        gmsh::model::mesh::getNodes(nodeTags, nodeCoords, nodeParams, dim, tag);

        // get the mesh elements for each elementary entity
        std::vector<int> elemTypes;
        std::vector<std::vector<std::size_t>> elemTags, elemNodeTags;
        gmsh::model::mesh::getElements(elemTypes, elemTags, elemNodeTags, dim, tag);

        // report some statistics
        int numElem = 0;
        for (unsigned int i = 0; i < elemTags.size(); i++)
            numElem += elemTags[i].size();
        std::string type;
        gmsh::model::getType(dim, tag, type);
        std::cout << nodeTags.size() << " mesh nodes and "
                  << numElem << " mesh elements on entity ("
                  << dim << "," << tag << ") of type \"" << type << "\"\n";
        std::vector<int> partitions;
        gmsh::model::getPartitions(dim, tag, partitions);
        if (partitions.size())
        {
            std::cout << " - Partition tag(s):";
            for (unsigned int i = 0; i < partitions.size(); i++)
                std::cout << " " << partitions[i];
            int parentDim, parentTag;
            gmsh::model::getParent(dim, tag, parentDim, parentTag);
            std::cout << " - parent entity (" << parentDim << "," << parentTag << ")\n";
        }
        for (unsigned int i = 0; i < elemTypes.size(); i++)
        {
            std::string name;
            int d, order, numv, numpv;
            std::vector<double> param;
            gmsh::model::mesh::getElementProperties(elemTypes[i], name, d, order,
                                                    numv, param, numpv);
            std::cout << " - Element type: \"" << name << "\", order " << order << "\n";
            std::cout << "   with " << numv << " nodes in param coord: (";
            for (unsigned int j = 0; j < param.size(); j++)
                std::cout << param[j] << " ";
            std::cout << ")\n";
        }
    }

    // ==================================================
    // explore physical groups

    gmsh::vectorpair dimTags;
    gmsh::model::getPhysicalGroups(dimTags);
    std::cout << "\nPhysical groups (size=" << dimTags.size() << "):\n";
    for (auto &p : dimTags)
    {
        std::string name;
        gmsh::model::getPhysicalName(p.first, p.second, name);
        std::cout << "\t- PhysicalGroup #(" << p.first << "," << p.second << ") named \"" << name << "\":\n";
        std::vector<int> tags;
        gmsh::model::getEntitiesForPhysicalGroup(p.first, p.second, tags);
        std::cout << "\t\tincludes tags: ";
        for (auto tag : tags)
            std::cout << tag << " ";
        std::cout << '\n';
    }

    // ===================================================

    // get type of element used in the mesh

    std::vector<int> eleTypes;
    gmsh::model::mesh::getElementTypes(eleTypes, 2);
    if (eleTypes.size() != 1)
    {
        gmsh::logger::write("Empty/Hybrid meshes not handled in this example!",
                            "error");
        return 1;
    }
    int elType = eleTypes[0];

    // get info about this type of element
    std::string name;
    int dim, order, numNodes, numPrimaryNodes;
    std::vector<double> paramCoord;
    gmsh::model::mesh::getElementProperties(elType, name, dim, order,
                                            numNodes, paramCoord, numPrimaryNodes);
    std::cout << "\t2D elements are of type=\"" << name << "\" (type=" << elType
              << "), order=" << order << " with " << numNodes << " nodes\n";

    // get integration points
    std::cout << "\nintegration points:\n";
    std::vector<double> intQ, intW;
    gmsh::model::mesh::getIntegrationPoints(elType, "Gauss4", intQ, intW);
    std::cout << "intQ.size()=" << intQ.size() << '\n';

    // get jacobians
    std::vector<double> jac, det, pts;
    gmsh::model::mesh::getJacobians(elType, intQ, jac, det, pts);
    std::cout << "jac.size()=" << jac.size() << '\n';
    std::cout << "det.size()=" << det.size() << '\n';
    std::cout << "pts.size()=" << pts.size() << '\n';
    // for (std::size_t i = 0; i < det.size(); i++)
    //     printf("det = %g\n", det[i]);


    gmsh::option::setNumber("Mesh.SurfaceFaces", 1);
    gmsh::option::setNumber("Mesh.Points", 1);
    gmsh::option::setNumber("General.Orthographic", 0);
    
    gmsh::fltk::run();

    gmsh::finalize();
    return 0;
}
