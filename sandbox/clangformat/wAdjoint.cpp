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

#include "wAdjoint.h"
#include "wProblem.h"
#include "wMedium.h"
#include "wAssign.h"
#include "wFreestream.h"
#include "wWake.h"
#include "wBoundary.h"
#include "wSolver.h"

#include "wMshData.h"
#include "wNode.h"
#include "wElement.h"
#include "wTag.h"
#include "wCache.h"
#include "wMem.h"
#include "wResults.h"
#include "wMshExport.h"
#include "wSparseLu.h"

#include <Eigen/Sparse>

#include <tbb/task_scheduler_init.h>
#include <tbb/tbb.h>
#include <tbb/parallel_do.h>
#include <tbb/spin_mutex.h>

#include <iomanip>

using namespace tbox;
using namespace flow;

#define ANSI_COLOR_YELLOW "\x1b[1;33m"
#define ANSI_COLOR_RESET "\x1b[0m"

/**
 * @brief Initialize the adjoint solver
 * @authors Adrien Crovato
 */
Adjoint::Adjoint(std::shared_ptr<Solver> _sol) : sol(_sol)
{
    // Default parameters
    nthreads = sol->nthreads;
    verbose = sol->verbose;

    // Setup variables (unkwown and residual vectors)
    lambdaL.resize(sol->pbl->msh->nodes.size(), 0.);
    lambdaD.resize(sol->pbl->msh->nodes.size(), 0.);
}

/**
 * @brief Run the linear solver
 * 
 * Solve the adjoint steady transonic Full Potential Equation
 * i.e. computes sensitivites for lift and drag cost functions
 * @authors Adrien Crovato
 */
void Adjoint::run()
{
    // Init
    tbb::task_scheduler_init init(nthreads);

    // Display solver parameters
    std::cout << "--- Adjoint solver ---\n"
              << "Number of threads: " << nthreads << "\n"
              << std::endl;
    // Map solution vectors
    Eigen::Map<Eigen::VectorXd> lambdaL_(lambdaL.data(), lambdaL.size()), lambdaD_(lambdaD.data(), lambdaD.size());
    // build SoE
    Eigen::SparseMatrix<double, Eigen::RowMajor> dR(sol->pbl->msh->nodes.size(), sol->pbl->msh->nodes.size());
    buildDR(dR);
    std::vector<double> dummyL(sol->pbl->msh->nodes.size()), dummyD(sol->pbl->msh->nodes.size()); // dummy bs vector
    Eigen::Map<Eigen::VectorXd> dL_(dummyL.data(), dummyL.size()), dD_(dummyD.data(), dummyD.size());
    buildDI(dL_, dD_);

    // Setup linear solver and solve linear SoE dR {lambdaL, lambdaD} = {dL, dD}
    SparseLu lu;
    lu.compute(dR, dL_, lambdaL_);
    lu.compute(dR, dD_, lambdaD_);

    // Check residual
    Eigen::VectorXd rLambdaL = dR * lambdaL_ - dL_;
    Eigen::VectorXd rLambdaD = dR * lambdaD_ - dD_;

    std::cout << std::setw(8) << "L_Iter"
              << std::setw(15) << "Sens[lambdaL]"
              << std::setw(15) << "Sens[lambdaD]"
              << std::setw(15) << "Res[lambdaL]"
              << std::setw(15) << "Res[lambdaD]" << std::endl;
    std::cout << std::fixed << std::setprecision(5);
    std::cout << std::setw(8) << 0
              << std::setw(15) << lambdaL_.norm()
              << std::setw(15) << lambdaD_.norm()
              << std::setw(15) << log10(rLambdaL.norm())
              << std::setw(15) << log10(rLambdaD.norm()) << std::endl;

    std::cout << ANSI_COLOR_YELLOW << "Warning: the adjoint solver is experimental and has not been validated!\n"
              << ANSI_COLOR_RESET << std::endl;
}

/**
 * @brief Build the Jacobian matrix
 * @authors Adrien Crovato
 * @todo Refactor, this is heavily copy-pasted from Newton::buildJ
 */
void Adjoint::buildDR(Eigen::SparseMatrix<double, Eigen::RowMajor> &dR)
{
    // Multithread
    tbb::spin_mutex mutex;

    // List of triplets to build Jacobian matrix
    std::deque<Eigen::Triplet<double>> T;

    // Full Potential Equation with upwind bias: analytical derivatives
    auto fluid = sol->pbl->medium;
    tbb::parallel_do(fluid->adjMap.begin(), fluid->adjMap.end(), [&](std::pair<Element *, std::vector<Element *>> p) {
        // Current element
        Element *e = p.first;
        //Upwind element
        Element *eU = p.second[0];

        // Subsonic contribution (drho*grad_phi*grad_psi + rho*grad_phi*grad_psi)
        Eigen::MatrixXd Ae1 = e->buildDK(sol->phi, fluid->rho);
        Eigen::MatrixXd Ae2 = e->buildK(sol->phi, fluid->rho);
        // Supersonic contribution
        double mCOv = 0.95; // solver should be converged, so we set the lowest numerical viscosity
        double muCv = 1.0;
        double mach = fluid->mach.eval(*e, sol->phi, 0);
        if (mach > mCOv)
        {
            // switching function and derivative
            double mu = muCv * (1 - (mCOv * mCOv) / (mach * mach));
            double dmu = 2 * muCv * mCOv * mCOv / (mach * mach * mach);

            // scale 1st and 2nd terms
            Ae1 *= 1 - mu;
            Ae2 *= 1 - mu;

            // 3rd term (mu*drhoU*grad_phi*grad_psi)
            Eigen::MatrixXd Ae3 = mu * fluid->rho.evalD(*eU, sol->phi, 0) * e->buildDs(sol->phi, Fct0C(1.)) * eU->computeGrad(sol->phi, 0).transpose() * eU->getVCache().getDsf(0);

            // 4th term (mu*rhoU*grad_phi*grad_psi)
            Eigen::MatrixXd Ae4 = mu * fluid->rho.eval(*eU, sol->phi, 0) * e->buildK(sol->phi, Fct0C(1.));

            // 5th term dmu*(rhoU-rho)*grad_phi*grad_psi
            Eigen::MatrixXd Ae5 = dmu * (fluid->rho.eval(*eU, sol->phi, 0) - fluid->rho.eval(*e, sol->phi, 0)) * e->buildDK(sol->phi, fluid->mach);

            // Assembly (supersonic)
            tbb::spin_mutex::scoped_lock lock(mutex);
            for (size_t ii = 0; ii < e->nodes.size(); ++ii)
            {
                Node *nodi = e->nodes[ii];
                for (size_t jj = 0; jj < e->nodes.size(); ++jj)
                {
                    Node *nodj = e->nodes[jj];
                    T.push_back(Eigen::Triplet<double>(nodi->row, nodj->row, Ae4(ii, jj) + Ae5(ii, jj)));
                }
                for (size_t jj = 0; jj < eU->nodes.size(); ++jj)
                {
                    Node *nodj = eU->nodes[jj];
                    T.push_back(Eigen::Triplet<double>(nodi->row, nodj->row, Ae3(ii, jj)));
                }
            }
        }
        // Assembly (subsonic)
        tbb::spin_mutex::scoped_lock lock(mutex);
        for (size_t ii = 0; ii < e->nodes.size(); ++ii)
        {
            Node *nodi = e->nodes[ii];
            for (size_t jj = 0; jj < e->nodes.size(); ++jj)
            {
                Node *nodj = e->nodes[jj];
                T.push_back(Eigen::Triplet<double>(nodi->row, nodj->row, Ae1(ii, jj) + Ae2(ii, jj)));
            }
        }
    });
    // Apply wake BCs for adjoint problem
    for (auto wake : sol->pbl->wBCs)
    {
        tbb::parallel_do(wake->wEle.begin(), wake->wEle.end(), [&](WakeElement *we) {
            Eigen::MatrixXd Kupup(we->nColUp, we->nRow);
            Eigen::MatrixXd Kuplw(we->nColUp, we->nRow);
            Eigen::MatrixXd Klwup(we->nColLw, we->nRow);
            Eigen::MatrixXd Klwlw(we->nColLw, we->nRow);
            buildWake(we, Kupup, Kuplw, Klwup, Klwlw);
            // Assembly
            tbb::spin_mutex::scoped_lock lock(mutex);
            for (size_t ii = 0; ii < we->nColUp; ++ii)
            {
                Node *nodi = we->volUpE->nodes[ii];
                for (size_t jj = 0; jj < we->nRow; ++jj)
                {
                    Node *nodj = we->surUpE->nodes[jj];
                    T.push_back(Eigen::Triplet<double>(nodi->row, nodj->row, 2 * Kupup(ii, jj)));
                    //dR.coeffRef(nodi->row, nodj->row) += 2 * Kupup(ii, jj);
                    nodj = we->surLwE->nodes[jj];
                    T.push_back(Eigen::Triplet<double>(nodi->row, nodj->row, Kuplw(ii, jj)));
                    //dR.coeffRef(nodi->row, nodj->row) += Kuplw(ii, jj);
                }
            }
            for (size_t ii = 0; ii < we->nColLw; ++ii)
            {
                Node *nodi = we->volLwE->nodes[ii];
                for (size_t jj = 0; jj < we->nRow; ++jj)
                {
                    Node *nodj = we->surUpE->nodes[jj];
                    //dR.coeffRef(nodi->row, nodj->row) -= 2 * Klwup(ii, jj);
                    T.push_back(Eigen::Triplet<double>(nodi->row, nodj->row, -2 * Klwup(ii, jj)));
                    nodj = we->surLwE->nodes[jj];
                    //dR.coeffRef(nodi->row, nodj->row) -= Klwlw(ii, jj);
                    T.push_back(Eigen::Triplet<double>(nodi->row, nodj->row, -Klwlw(ii, jj)));
                }
            }
        });
    }
    // Build Jacobian matrix without BCs
    dR.setFromTriplets(T.begin(), T.end());
    // Apply Dirichlet BCs
    for (auto dBC : sol->pbl->dBCs)
    {
        for (auto nod : dBC->nodes)
        {
            for (Eigen::SparseMatrix<double, Eigen::RowMajor>::InnerIterator it(dR, nod->row); it; ++it)
            {
                if (it.row() == it.col())
                    it.valueRef() = 1.;
                else
                    it.valueRef() = 0.;
            }
        }
    }
    for (auto fBC : sol->pbl->fBCs)
    {
        for (auto e : fBC->tag->elems)
        {
            for (auto nod : e->nodes)
            {
                for (Eigen::SparseMatrix<double, Eigen::RowMajor>::InnerIterator it(dR, nod->row); it; ++it)
                {
                    if (it.row() == it.col())
                        it.valueRef() = 1.;
                    else
                        it.valueRef() = 0.;
                }
            }
        }
    }
    // Transpose
    dR.transpose();
    // Clean matrix and turn to compressed row format
    dR.prune(0.);
    dR.makeCompressed();

    if (verbose)
        std::cout << "J (" << dR.rows() << "," << dR.cols() << ") nnz=" << dR.nonZeros() << "\n";
}

/**
 * @brief Build the derivatives of the cost functions
 * @authors Adrien Crovato
 * @todo Refactor, to be checked
 */
void Adjoint::buildDI(Eigen::Map<Eigen::VectorXd> &dL, Eigen::Map<Eigen::VectorXd> &dD)
{
    // Multithread
    tbb::spin_mutex mutex;

    // Derivative of lift and drag
    for (auto sur : sol->pbl->bnd)
    {
        tbb::parallel_do(sur->groups[0]->tag->elems.begin(), sur->groups[0]->tag->elems.end(), [&](Element *e) {
            // Build flux factor
            Element *eV = sur->svMap.at(e);
            Eigen::RowVectorXd be(eV->nodes.size());
            buildBoundary(e, eV, be);
            // Compute scaling factor
            Eigen::Vector3d Vi(0., 0., 0.);
            Vi(0) = -sin(sol->pbl->alpha);
            Vi(sol->pbl->nDim - 1) = cos(sol->pbl->alpha);
            double factorL = -1 / sol->pbl->S_ref * sol->pbl->medium->cP.evalD(*eV, sol->phi, 0) * e->normal().dot(Vi);
            Vi(0) = cos(sol->pbl->alpha);
            Vi(sol->pbl->nDim - 1) = sin(sol->pbl->alpha);
            double factorD = -1 / sol->pbl->S_ref * sol->pbl->medium->cP.evalD(*eV, sol->phi, 0) * e->normal().dot(Vi);

            // Assembly
            tbb::spin_mutex::scoped_lock lock(mutex);
            for (size_t ii = 0; ii < eV->nodes.size(); ++ii)
            {
                Node *nodi = eV->nodes[ii];
                dL(nodi->row) += factorL * be(ii);
                dD(nodi->row) += factorD * be(ii);
            }
        });
    }
    // Apply Dirichlet BCs, adjoint solution vanishes at infinity
    for (auto dBC : sol->pbl->dBCs)
    {
        for (auto nod : dBC->nodes)
        {
            dL(nod->row) = 0.;
            dD(nod->row) = 0.;
        }
    }
    for (auto fBC : sol->pbl->fBCs)
    {
        for (auto e : fBC->tag->elems)
        {
            for (auto nod : e->nodes)
            {
                dL(nod->row) = 0.;
                dD(nod->row) = 0.;
            }
        }
    }

    if (verbose)
    {
        std::cout << "dL (" << dL.size() << ")\n";
        std::cout << "dD (" << dD.size() << ")\n";
    }
}

/**
 * @brief Build the boundary contribution for the adjoint equation
 * @authors Adrien Crovato
 * @todo Refactor, to be checked and moved to Boundary::buildAdjNs()
 */
void Adjoint::buildBoundary(Element *&e, Element *&eV, Eigen::RowVectorXd &be)
{
    // Get shape functions
    Eigen::MatrixXd const &volDff = eV->getVCache().getDsf(0);
    // Get Jacobian
    Mem &surMem = e->getVMem();
    Mem &volMem = eV->getVMem();
    // b = V*[inv(J)*dN]
    be = eV->computeGrad(sol->phi, 0).transpose() * volMem.getJinv(0) * volDff * surMem.getVol();
}

/**
 * @brief Build the wake contribution for the adjoint equation
 * @authors Adrien Crovato
 * @todo Refactor, to be checked and moved to WakeElement::buildAdjNK()
 */
void Adjoint::buildWake(WakeElement *&we, Eigen::MatrixXd &Kupup, Eigen::MatrixXd &Kuplw, Eigen::MatrixXd &Klwup, Eigen::MatrixXd &Klwlw)
{
    // Get shape functions and Gauss points
    Cache &surCacheUp = we->surUpE->getVCache();
    Gauss &surGaussUp = surCacheUp.getVGauss();
    Cache &surCacheLw = we->surLwE->getVCache();
    Gauss &surGaussLw = surCacheLw.getVGauss();
    Eigen::MatrixXd const &volUpDff = we->volUpE->getVCache().getDsf(0);
    Eigen::MatrixXd const &volLwDff = we->volLwE->getVCache().getDsf(0);
    // Get Jacobian
    Mem &surMemUp = we->surUpE->getVMem();
    Mem &surMemLw = we->surLwE->getVMem();
    Eigen::MatrixXd const &volUpJ = we->volUpE->getVMem().getJinv(0);
    Eigen::MatrixXd const &volLwJ = we->volLwE->getVMem().getJinv(0);
    // Reset matrices
    Kupup = Eigen::MatrixXd::Zero(we->nColUp, we->nRow);
    Kuplw = Eigen::MatrixXd::Zero(we->nColUp, we->nRow);
    Klwup = Eigen::MatrixXd::Zero(we->nColLw, we->nRow);
    Klwlw = Eigen::MatrixXd::Zero(we->nColLw, we->nRow);

    // unit normal
    Eigen::VectorXd nUp = Eigen::VectorXd::Zero(sol->pbl->nDim);
    Eigen::VectorXd nLw = Eigen::VectorXd::Zero(sol->pbl->nDim);
    nUp(0) = we->surUpE->normal()(0);
    nUp(sol->pbl->nDim - 1) = we->surUpE->normal()(sol->pbl->nDim - 1);
    nLw(0) = we->surLwE->normal()(0);
    nLw(sol->pbl->nDim - 1) = we->surLwE->normal()(sol->pbl->nDim - 1);
    // intermediate vectors
    Eigen::VectorXd nJdNUp(we->nColUp);
    Eigen::VectorXd nJdNLw(we->nColLw);
    Eigen::VectorXd VJdNUp(we->nColUp);
    Eigen::VectorXd VJdNLw(we->nColLw);
    nJdNUp = volUpDff * volUpJ.transpose() * nUp;
    nJdNLw = volLwDff * volLwJ.transpose() * nLw;
    VJdNUp = volUpDff * volUpJ.transpose() * we->volUpE->computeGrad(sol->phi, 0);
    VJdNLw = volLwDff * volLwJ.transpose() * we->volLwE->computeGrad(sol->phi, 0);

    // Build
    /* @todo assemble on contributing nodes for lower surfaces => nRow <- we->surE->nodes.size() */
    for (size_t k = 0; k < surGaussUp.getN(); ++k)
    {
        // N
        Eigen::RowVectorXd Nup = surCacheUp.getSf(k).transpose();
        Eigen::RowVectorXd Nlw = surCacheLw.getSf(k).transpose();
        // K = K + N*VJdN*w*dtm
        Kupup += VJdNUp * Nup * surGaussUp.getW(k) * surMemUp.getDetJ(k);
        Klwup += VJdNLw * Nup * surGaussUp.getW(k) * surMemUp.getDetJ(k);
        // K = K + N*nJdN*w*dtm
        Kuplw += nJdNUp * Nlw * surGaussLw.getW(k) * surMemLw.getDetJ(k);
        Klwlw += nJdNLw * Nlw * surGaussLw.getW(k) * surMemLw.getDetJ(k);
    }
}

/**
 * @brief Write the results
 * @authors Adrien Crovato
 */
void Adjoint::save(int n, std::shared_ptr<MshExport> mshWriter)
{
    // Write files
    std::cout << "Saving files... " << std::endl;
    // setup results
    Results results;
    results.scalars_at_nodes["lambdaL"] = &lambdaL;
    results.scalars_at_nodes["lambdaD"] = &lambdaD;
    // save (all mesh and boundary surface)
    if (n > 0)
    {
        mshWriter->save(sol->pbl->msh->name + "_adjoint_" + std::to_string(n), results);
        for (auto sur : sol->pbl->bnd)
            sur->save(sur->groups[0]->tag->name + "adjoint_" + std::to_string(n), results);
    }
    else
    {
        mshWriter->save(sol->pbl->msh->name + "_adjoint", results);
        for (auto sur : sol->pbl->bnd)
            sur->save(sur->groups[0]->tag->name + "_adjoint", results);
    }
}

void Adjoint::write(std::ostream &out) const
{
    out << "flow::Adjoint"
        << "\n";
}
