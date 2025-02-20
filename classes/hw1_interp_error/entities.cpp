#include "entities.h"
#include "groups.h"
#include <iostream>
#include <cassert>
#include <gmsh.h>

void fillEntities(PhysicalGroups const &groups,
                 std::string const &groupname,
                 std::vector<Entity> &entities,
                 std::map<int, ElemPrp> &prps)
{
    auto it = groups.dimTags.find(groupname);
    if (it == groups.dimTags.end())
    {
        std::cerr << "Group '" << groupname << "' does not exist!\n";
        exit(EXIT_FAILURE);
    }
    int dim = it->second.first;
    assert(dim == 2);
    int tag = it->second.second;

    // get entities of the chosen physical group
    std::vector<int> etags;
    gmsh::model::getEntitiesForPhysicalGroup(dim, tag, etags);
    if (verbosity > VERB::INFO)
    {
        std::cout << "Entities in group named '" << groupname << "': ";
        for (size_t i = 0; i < etags.size(); ++i)
            std::cout << etags[i] << " ";
        std::cout << '\n';
    }
    // loop over entities
    for (size_t ientity = 0; ientity < etags.size(); ++ientity)
    {
        Entity entity;
        entity.tag = etags[ientity];
        if (verbosity > VERB::INFO)
            std::cout << "Entity (" << dim << "," << entity.tag << "):\n";
        gmsh::model::mesh::getElements(entity.elementTypes, entity.elementTags,
                                       entity.nodeTags, dim, entity.tag);
        entities.push_back(entity);

        // loop over element types
        for (size_t ieltyp = 0; ieltyp < entity.elementTypes.size(); ++ieltyp)
        {
            int eltyp = entity.elementTypes[ieltyp];
            // get info about this type of element
            auto it = prps.find(ieltyp);
            if (it == prps.end())
            {
                // does not exist
                ElemPrp prp;
                prp.type = eltyp;
                gmsh::model::mesh::getElementProperties(prp.type,
                                                        prp.name,
                                                        prp.dim,
                                                        prp.order,
                                                        prp.numNodes,
                                                        prp.paramCoord,
                                                        prp.numPrimaryNodes);

                gmsh::model::mesh::getIntegrationPoints(prp.type,
                                                        //"CompositeGauss20", // "Gauss2"
                                                        "Gauss20", // "Gauss2"
                                                        prp.localCoords,
                                                        prp.weights);
                // std::cout << getLocalCoords(prp).format(fmt) << '\n';

                int numComponents, numOrientations;
                gmsh::model::mesh::getBasisFunctions(prp.type, prp.localCoords,
                                                     "Lagrange", numComponents,
                                                     prp.basisF,
                                                     numOrientations);

                // gmsh::model::mesh::getBasisFunctions(prp.type,
                //                                      prp.localCoords,
                //                                      "GradLagrange", numComponents,
                //                                      prp.basisDF,
                //                                      numOrientations);
                // std::cout << "new element type: " << prp.name << '\n';
                prps[eltyp] = prp;
            }
            ElemPrp &prp = prps[eltyp];
            if (verbosity > VERB::INFO)
                std::cout << "\telement type: \"" << prp.name
                          << "\" with " << entity.elementTags[ieltyp].size() << " elements and "
                          << entity.nodeTags[ieltyp].size() << " nodes"
                          << " using " << prp.weights.size() << " GPs"
                          << '\n';
                          
        }
    }
}
