#include "interpolate.h"
#include "entities.h"
#include "nodes.h"
#include "func.h"
#include "eigenmaps.h"
#include <iostream>

void interpolate(Nodes &nodes, std::vector<Entity> &entities,
                 std::map<int, ElemPrp> &prps,
                 std::vector<std::size_t> &elems2D,
                 std::vector<std::vector<double>> &nodalF,
                 std::string &fct)

{
    // compute nodal data
    if (verbosity > VERB::INFO)
        std::cout << "interpolating function f..." << std::endl;
    int idx = 0;
    for (auto &entity : entities)
    {
        for (size_t i = 0; i < entity.elementTypes.size(); ++i) // ith element type of "entity"
        {
            int type = entity.elementTypes[i];
            int numNodes = prps[type].numNodes;
            for (size_t j = 0; j < entity.nodeTags[i].size() / numNodes; ++j) // jth element of type i
            {
                // std::size_t eltag = entity.elementTags[i][j]; // for print
                // std::cout << idx << ": element #" << eltag << ": ";
                nodalF[idx].resize(numNodes);
                for (int n = 0; n < numNodes; ++n)
                {
                    size_t nodeTag = entity.nodeTags[i][j * numNodes + n];
                    // std::cout << nodeTag << " ";
                    // get coordinates of node nodeTag
                    size_t nidx = nodes.nodeIdx[nodeTag];
                    double x = nodes.coords[nidx * 3];
                    double y = nodes.coords[nidx * 3 + 1];
                    double z = nodes.coords[nidx * 3 + 2];
                    double f = func(fct, x, y, z); // evaluate fct at node "nodeTag"
                    nodalF[idx][n] = f;            // store value
                }
                // std::cout << '\n';
                idx++;
            }
        }
    }
}

void approximate(std::vector<Entity> &entities,
                 std::map<int, ElemPrp> &prps,
                 std::vector<std::size_t> &elems2D,
                 std::vector<std::tuple<size_t, size_t, size_t>> &elemLOCs,
                 std::vector<std::vector<std::vector<double>>> &determinants,
                 std::vector<std::vector<std::vector<double>>> &gpcoords,
                 std::vector<std::vector<double>> &nodalF,
                 std::string &fct)
{
    if (verbosity > VERB::INFO)
        std::cout << "approximating function f..." << std::endl;

    
#pragma omp parallel for
    for (size_t ne = 0; ne < elems2D.size(); ++ne) // loop over elements (could be done in //)
    {
        const auto [ientity, imsh, iel] = elemLOCs[ne];
        Entity &entity = entities[ientity];
        int type = entity.elementTypes[imsh];
        ElemPrp &prp = prps[type];

        size_t nGP = prp.weights.size();

        // get basis fcts
        auto basisF = getBasisF(prp);

        // build GP coordinates matrix
        Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>>
            gpXYZ(&gpcoords[ientity][imsh][iel * nGP * 3], nGP, 3);

        Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>>
            detJs(&determinants[ientity][imsh][0], determinants[ientity][imsh].size(), nGP);

        Eigen::MatrixXd M = Eigen::MatrixXd::Zero(prp.numNodes, prp.numNodes);
        Eigen::VectorXd f = Eigen::VectorXd::Zero(prp.numNodes);

        // loop over Gauss points

        for (size_t igp = 0; igp < nGP; ++igp)
        {
            auto N = basisF.row(igp); // vector of shape functions at GP #igp; shape=(1,prp.numNodes)
            M += N.transpose() * N * detJs(iel, igp) * prp.weights[igp];
            double fgp_exact = func(fct, gpXYZ(igp, 0), gpXYZ(igp, 1), gpXYZ(igp, 2));
            f += N.transpose() * fgp_exact * detJs(iel, igp) * prp.weights[igp];
        }
        // std::cout << "M=" << M.format(fmt) << '\n';
        // std::cout << "f=" << f.format(fmt) << '\n';
        // std::cout << "det(M) = " << M.determinant() << '\n';
        Eigen::VectorXd fa = M.colPivHouseholderQr().solve(f);
        // std::cout << "fa=" << fa.format(fmt) << '\n';
        nodalF[ne].resize(prp.numNodes);
        for (int n = 0; n < prp.numNodes; ++n)
            nodalF[ne][n] = fa(n);

        //exit(1);
    }
}
