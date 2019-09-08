//   Copyright 2003-2019 Romain Boman
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

#ifndef MESHER_H
#define MESHER_H

#include "genmai.h"
#include "gmObject.h"
#include <vector>
#include "Point.h"
#include "Element.h"
#include "LayerType.h"
class Mesh;

/**
 * @brief Fills a Mesh with the description of a horizontal sheet of metal.
 *        2D Quad elements are used. The mesh is described as a set of Layers.
 *        Parameters are defined in a MeshParameters object.
 */

class GENMAI_API MeshBuilder : public Object
{
    Mesh &target;

public:
    Point origin;
    Point dimension;
    int numberOfElementOnX;
    int numberOfElementOnY;
    double reductionCoefficient;
    std::vector<LayerType> layers;

  public:
    MeshBuilder(Mesh &_target);
    void genere();

  private:
    double currentHeight; ///< ordonnee courante
    double dx;            ///< largeur des mailles courantes
    int first;            ///< no du premier noeud de la ligne
    int last;             ///< no du dernier noeud de la ligne

    double computeBoundaryHeight();
    double computeReductionFactor();
    void meshFirstLine();
    void meshGradient();
    void meshGradientLayer(int lev);
    double getGradientDelta(int lev);
    void meshBoundary();
    void meshReductionLayer();
    void addReductionNodes();
    void addReductionElements();
    void meshConstantLayer();
    void addConstantNodes();
    void addConstantElements();
    void setContactNodes(int first, int last);
    void increaseHeight(double dh);
    void initialize();
};

#include "MeshBuilder.inl"

#endif
