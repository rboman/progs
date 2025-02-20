#include "elements.h"
#include "entities.h"
#include <gmsh.h>
#include <iostream>

size_t countNbOfElements(std::vector<Entity> &entities)
{
    size_t nbelems = 0;
    for (auto &entity : entities)
    {
        for (size_t i = 0; i < entity.elementTags.size(); ++i)
            nbelems += entity.elementTags[i].size();
    }

    if (verbosity > VERB::INFO)
        std::cout << "total number of elements: " << nbelems << '\n';

    return nbelems;
}

void fillElementData(ElementData &eldata,
                     std::vector<Entity> &entities,
                     std::map<int, ElemPrp> &prps,
                     std::vector<std::size_t> &elems2D,
                     std::vector<std::tuple<size_t, size_t, size_t>> &elemLOCs)
{
    eldata.jacobians.resize(entities.size());
    eldata.determinants.resize(entities.size());
    eldata.gpcoords.resize(entities.size());

    // fill vector of element tags (used by the view)
    if (verbosity > VERB::INFO)
        std::cout << "fill element data..." << std::endl;

    int idx = 0;
    for (size_t k = 0; k < entities.size(); ++k)
    {
        Entity &entity = entities[k];
        size_t ntypes = entity.elementTypes.size();
        eldata.jacobians[k].resize(ntypes);
        eldata.determinants[k].resize(ntypes);
        eldata.gpcoords[k].resize(ntypes);
        for (size_t i = 0; i < ntypes; ++i) // ith element type of "entity"
        {
            for (size_t j = 0; j < entity.elementTags[i].size(); ++j) // jth element of type i
            {
                std::size_t eltag = entity.elementTags[i][j];
                elems2D[idx] = eltag;
                elemLOCs[idx] = std::make_tuple(k, i, j); // store, for each element, the parent entity index, the parent mesh and its index in the elementset
                idx++;
            }
            // fill jacobians at GPs
            int elType = entity.elementTypes[i];
            ElemPrp &prp = prps[elType];
            gmsh::model::mesh::getJacobians(elType,
                                            prp.localCoords,
                                            eldata.jacobians[k][i],
                                            eldata.determinants[k][i],
                                            eldata.gpcoords[k][i],
                                            entity.tag);
        }
    }
}