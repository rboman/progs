#include "l2error.h"
#include "entities.h"
#include "eigenmaps.h"
#include "func.h"
#include <iostream>

double l2error(std::vector<Entity> &entities,
               std::map<int, ElemPrp> &prps,
               std::vector<std::size_t> &elems2D,
               std::vector<std::tuple<size_t, size_t, size_t>> &elemLOCs,
               std::vector<std::vector<std::vector<double>>> &determinants,
               std::vector<std::vector<std::vector<double>>> &gpcoords,
               std::vector<std::vector<double>> &nodalF,
               std::string &fct)
{
    if (verbosity > VERB::INFO)
        std::cout << "computing L2-norm of the error..." << std::endl;

    double Etot = 0.0;
#pragma omp parallel for
    for (size_t ne = 0; ne < elems2D.size(); ++ne) // loop over elements (could be done in //)
    {
        const auto [ientity, imsh, iel] = elemLOCs[ne];
        Entity &entity = entities[ientity];
        int type = entity.elementTypes[imsh];
        ElemPrp &prp = prps[type];
        // int numNodes = prp.numNodes;
        size_t nGP = prp.weights.size();
        // std::cout << ne << ": " << ientity << ' ' << imsh << ' ' << iel << " '" << prp.name << "'\n";

        // get basis fcts
        auto basisF = getBasisF(prp);
        // std::cout << "basisF=" << basisF.format(fmt) << '\n';

        Eigen::Map<Eigen::Vector<double, Eigen::Dynamic>> f(&nodalF[ne][0], nodalF[ne].size());
        // std::cout << "f=" << f.format(fmt) << '\n';

        // build GP coordinates matrix
        Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>>
            gpXYZ(&gpcoords[ientity][imsh][iel * nGP * 3], nGP, 3);

        Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>>
            detJs(&determinants[ientity][imsh][0], determinants[ientity][imsh].size(), nGP);

        // loop over Gauss points
        double Ee = 0.0;
        for (size_t igp = 0; igp < nGP; ++igp)
        {
            // std::cout << "igp=" << igp << '\n';
            auto N = basisF.row(igp);
            auto fgp = N * f;
            // std::cout << "N.f=" << fgp.format(fmt) << '\n';
            //

            double fgp_exact = func(fct, gpXYZ(igp, 0), gpXYZ(igp, 1), gpXYZ(igp, 2));
            // std::cout << "fgp_exact=" << fgp_exact << '\n';
            Ee += (fgp[0] - fgp_exact) * (fgp[0] - fgp_exact) * detJs(iel, igp) * prp.weights[igp];
            // std::cout << "detJ=" << detJs(iel, igp) << '\n';
        }
        // std::cout << "Ee = " << Ee << " Etot = " << Etot << '\n';
        Etot += Ee;
    }

    return sqrt(Etot);
}

double totalArea(std::vector<Entity> &entities,
                 std::map<int, ElemPrp> &prps,
                 std::vector<std::size_t> &elems2D,
                 std::vector<std::tuple<size_t, size_t, size_t>> &elemLOCs,
                 std::vector<std::vector<std::vector<double>>> &determinants)
{
    if (verbosity > VERB::INFO)
        std::cout << "computing mean area of the elements..." << std::endl;

    double Atot = 0.0;
#pragma omp parallel for
    for (size_t ne = 0; ne < elems2D.size(); ++ne) // loop over elements (could be done in //)
    {
        const auto [ientity, imsh, iel] = elemLOCs[ne];
        Entity &entity = entities[ientity];
        int type = entity.elementTypes[imsh];
        ElemPrp &prp = prps[type];

        size_t nGP = prp.weights.size();

        Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>>
            detJs(&determinants[ientity][imsh][0], determinants[ientity][imsh].size(), nGP);

        // loop over Gauss points
        double Ae = 0.0;
        for (size_t igp = 0; igp < nGP; ++igp)
            Ae += detJs(iel, igp) * prp.weights[igp];

        Atot += Ae;
    }
    if (verbosity > VERB::FATAL_ERROR)
    {
        std::cout << "Atot = " << Atot << '\n';
    }
    return Atot;
}
