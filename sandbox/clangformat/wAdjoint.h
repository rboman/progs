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

#ifndef WADJOINT_H
#define WADJOINT_H

#include "flow.h"
#include "wObject.h"

#include <iostream>
#include <vector>
#include <memory>
#include <Eigen/Sparse>

namespace flow
{

/**
 * @brief Adjoint solver class
 * @authors Adrien Crovato
 * @todo THIS CLASS IS EXPERIMENTAL, to check, clean, and refactor
 */
class FLOW_API Adjoint : public fwk::wSharedObject
{
public:
    std::shared_ptr<Solver> sol; ///< solver

    int nthreads; ///< number of threads for the assembly
    bool verbose; ///< display more info

    std::vector<double> lambdaL; ///< lift sensitivities
    std::vector<double> lambdaD; ///< drag sensitivities

public:
    Adjoint(std::shared_ptr<Solver> _sol);
    ~Adjoint() { std::cout << "~Adjoint()\n"; }

    void run();
    void save(int n, std::shared_ptr<tbox::MshExport> mshWriter);

#ifndef SWIG
    virtual void write(std::ostream &out) const override;
#endif

private:
    void buildDR(Eigen::SparseMatrix<double, Eigen::RowMajor> &dR);
    void buildDI(Eigen::Map<Eigen::VectorXd> &dL, Eigen::Map<Eigen::VectorXd> &dD);
    void buildWake(WakeElement *&we, Eigen::MatrixXd &Kupup, Eigen::MatrixXd &Kuplw, Eigen::MatrixXd &Klwup,
                   Eigen::MatrixXd &Klwlw);
    void buildBoundary(tbox::Element *&e, tbox::Element *&eV, Eigen::RowVectorXd &be);
};

} // namespace flow
#endif // WADJOINT_H
