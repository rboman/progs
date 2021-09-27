/*
 * Copyright 2020 University of Liège
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "wSorter.h"
#include "wKernel.h"
#include "wParticle.h"
#include "wDofs.h"
#include <algorithm>
#include <sstream>
#include <exception>
#include <tbb/parallel_for.h>
using namespace sph;

Sorter::Sorter() : nthreads(1)
{
}

Sorter::~Sorter()
{
}

void Sorter::write(std::ostream &out) const
{
    out << "sph::Sorter";
}

// ------------------------------------

LouisSorter::LouisSorter(double _edgeL, double _hmax, Kernel const &kernel) : edgeL(_edgeL), hmax(_hmax)
{
    kappa = kernel.kappa();
    ncells = std::max(int(edgeL / (hmax * kappa)), 1);
}

LouisSorter::~LouisSorter()
{
    std::cout << "~LouisSorter()\n";
}

void LouisSorter::write(std::ostream &out) const
{
    out << "sph::LouisSorter:\n";
    out << "\tedgeL  = " << edgeL << '\n';
    out << "\thmax   = " << hmax << '\n';
    out << "\tncells = " << ncells << 'x' << ncells << 'x' << ncells;
}

void LouisSorter::execute(std::vector<Particle *> &prts, int step)
{
    // alloc if required
    if (sgrid.empty())
        sgrid.resize(ncells * ncells * ncells);
    else
        for (auto &l : sgrid)
            l.clear();

    // fills the grid
    for (auto p : prts)
    {
        Eigen::Vector3d &x = p->dofs[step]->x;
        int ii[3];
        for (int i = 0; i < 3; ++i)
        {
            int id = int((x(i) / edgeL) * ncells);
            if (id < 0)
                id = 0;
            if (id >= ncells)
                id = ncells - 1;
            ii[i] = id;
        }
        sgrid[idx(ii[0], ii[1], ii[2])].push_back(p);
    }

    // fills neighbours
    double Rmax = (hmax * kappa);
    double R2max = Rmax * Rmax;

    for (auto p : prts)
    {
        p->neighs.clear();
        Eigen::Vector3d &x = p->dofs[step]->x;
        int binf[3], bsup[3];
        for (int i = 0; i < 3; ++i)
        {
            int id = int((x(i) / edgeL) * ncells);
            if (id < 0)
                id = 0;
            if (id >= ncells)
                id = ncells - 1;
            binf[i] = decidx(id);
            bsup[i] = incidx(id);
        }
        for (int i = binf[0]; i <= bsup[0]; ++i)
            for (int j = binf[1]; j <= bsup[1]; ++j)
                for (int k = binf[2]; k <= bsup[2]; ++k)
                    for (auto pn : sgrid[idx(i, j, k)])
                        if (pn != p)
                        {
                            Eigen::Vector3d &xn = pn->dofs[step]->x;
                            double R2 = (xn - x).squaredNorm();
                            if (R2 <= R2max)
                                p->neighs.push_back(pn);
                        }
    }
}

void LouisSorter::chkidx(int i, int j, int k) const
{
    if (i < 0 || i >= ncells || j < 0 || j >= ncells || k < 0 || k >= ncells)
    {
        std::stringstream str;
        str << "LouisSorter:: bad index (" << i << "," << j << "," << k << ") [ncells=" << ncells << "]!";
        throw std::runtime_error(str.str());
    }
}

// ------------------------------------

BruteForceSorter::BruteForceSorter(double _hmax, Kernel const &kernel) : hmax(_hmax)
{
    kappa = kernel.kappa();
    method = 1;
}

BruteForceSorter::~BruteForceSorter()
{
    std::cout << "~BruteForceSorter()\n";
}

void BruteForceSorter::write(std::ostream &out) const
{
    out << "sph::BruteForceSorter:\n";
    out << "\thmax   = " << hmax << '\n';
}

void BruteForceSorter::execute(std::vector<Particle *> &prts, int step)
{
    for (auto p : prts)
        p->neighs.clear();
    //p->neighs.reserve(100); // change rien

    double Rmax = (hmax * kappa);
    double R2max = Rmax * Rmax;

    if (method == 1)
    {
        std::cout << "using serial code\n";
        for (auto it = prts.begin(); (it + 1) != prts.end(); ++it)
        {
            Particle *p = *it;
            Eigen::Vector3d &x = p->dofs[step]->x;
            for (auto it2 = it + 1; it2 != prts.end(); ++it2)
            {
                Particle *pn = *it2;
                Eigen::Vector3d &xn = pn->dofs[step]->x;
                double R2 = (xn - x).squaredNorm();
                if (R2 <= R2max)
                {
                    p->neighs.push_back(pn);
                    pn->neighs.push_back(p);
                }
            }
        }
    }
    else //if(method==2)
    {
        std::cout << "using parallel code with " << nthreads << " threads\n";
        // (NAIVE) PARALLEL CODE

        int grainsize = 200;
        tbb::parallel_for(
            tbb::blocked_range<size_t>(0, prts.size(), grainsize),
            [=](tbb::blocked_range<size_t> &r) {
                //std::cout << r.size() << '\n';
                for (size_t i = r.begin(); i < r.end(); ++i)
                {
                    Particle *p = prts[i];
                    Eigen::Vector3d &x = p->dofs[step]->x;
                    //Eigen::Vector3d x = p->dofs[step]->x;
                    for (auto it2 = prts.begin(); it2 != prts.end(); ++it2)
                    {
                        Particle *pn = *it2;
                        Eigen::Vector3d &xn = pn->dofs[step]->x;
                        double R2 = (xn - x).squaredNorm();
                        if (R2 <= R2max)
                            p->neighs.push_back(pn);
                    }
                }
            },
            tbb::simple_partitioner());
    }
}
