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

#ifndef WSORTER_H
#define WSORTER_H

#include "sph.h"
#include "wObject.h"
#include <iostream>
#include <vector>
#include <memory>

namespace sph
{

/**
 * @brief Nearest neighbour search
 */

class SPH_API Sorter : public fwk::wSharedObject
{
public:
    int nthreads; //< nb of threads

    Sorter();
    virtual ~Sorter();

    virtual void execute(std::vector<Particle *> &parts, int step = 0) = 0;
#ifndef SWIG
    virtual void write(std::ostream &out) const override;
#endif
};

/**
 * @brief Louis'method: particles are sorted in a cubic structured grid 
 *        between (x,y,z)=(0,0,0) and (x,y,z)=(edgeL,edgeL,edgeL)
 */

class SPH_API LouisSorter : public Sorter
{
    std::vector<std::vector<Particle *>> sgrid; //< structured grid
    double kappa;

public:
    double edgeL; //< length of the cube edge
    double hmax;  //< expected max smothing length used to calculate the number of elements in the sgrid
    int ncells;   //< number of cells along 1 edge

    LouisSorter(double _edgeL, double _hmax, Kernel const &kernel);
    virtual ~LouisSorter();
    virtual void execute(std::vector<Particle *> &prts, int step = 0) override;

    // debug (only)
    size_t nbprts(int i, int j, int k) const
    {
        chkidx(i, j, k);
        return sgrid[idx(i, j, k)].size();
    }
    std::vector<Particle *> &getParticles(int i, int j, int k)
    {
        chkidx(i, j, k);
        return sgrid[idx(i, j, k)];
    }
    int size() const { return ncells; }
#ifndef SWIG
    virtual void write(std::ostream &out) const override;
#endif
private:
    int idx(int i, int j, int k) const { return ncells * (ncells * i + j) + k; }
    void chkidx(int i, int j, int k) const;
    int incidx(int id) const
    {
        ++id;
        if (id >= ncells)
            id = ncells - 1;
        return id;
    }
    int decidx(int id) const
    {
        --id;
        if (id < 0)
            id = 0;
        return id;
    }
};

/**
 * @brief Brute force neighbour search (used for comparison)
 */

class SPH_API BruteForceSorter : public Sorter
{
    double kappa;

public:
    double hmax; //< expected max smothing length used to calculate the number of elements in the sgrid
    int method;

    BruteForceSorter(double _hmax, Kernel const &kernel);
    virtual ~BruteForceSorter();
    virtual void execute(std::vector<Particle *> &prts, int step = 0) override;

#ifndef SWIG
    virtual void write(std::ostream &out) const override;
#endif
};

} // namespace sph

#endif //WSORTER_H
