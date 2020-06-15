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

#include "gmMeshBuilder.h"
#include "gmMesh.h"
#include <stdexcept>

using namespace genmai;

MeshBuilder::MeshBuilder(Mesh &_target) : Object(), target(_target)
{
    // default parameters
    origin.x = -2.0;
    origin.y = -0.5175 / 2.0;
    dimension.x = 2.0;
    dimension.y = 0.5175 / 2.0;
    numberOfElementOnX = 40;
    numberOfElementOnY = 2;
    reductionCoefficient = 5.0;

    layers.push_back(REDUCTION);
    layers.push_back(REDUCTION);
    layers.push_back(REDUCTION);
    layers.push_back(CONSTANT);
}

void
MeshBuilder::write(std::ostream &out) const
{
    out << "MeshBuilder:\n";
    out << "\torigin               : " << origin << '\n';
    out << "\tdimension            : " << dimension << '\n';
    out << "\tnumberOfElementOnX   : " << numberOfElementOnX << '\n';
    out << "\tnumberOfElementOnY   : " << numberOfElementOnY << '\n';
    out << "\treductionCoefficient : " << reductionCoefficient << '\n';
    out << "\tlayers               : ";
    for (auto l : layers)
        out << l << ' ';
    out << '\n';
}

/**
 * @brief Calcul la hauteur de la zone "boundary"
 */

double
MeshBuilder::computeBoundaryHeight()
{
    double dx2 = dimension.x / numberOfElementOnX;

    double hcl = dx2;
    for (int i = 1; i < layers.size(); i++)
    {
        if (layers[i - 1] == REDUCTION)
            dx2 /= 2.0;
        hcl += dx2;
    }

    // verification
    if (hcl > dimension.y)
    {
        throw std::runtime_error("erreur: hauteur CL > hauteur totale");
    }

    return hcl;
}

/**
 * @brief Calcul du facteur du gradient
 */

double
MeshBuilder::computeReductionFactor()
{
    double alp = 0.0;

    if (numberOfElementOnY > 1)
    {
        for (int i = 0; i < numberOfElementOnY; i++)
            alp += 1.0 +
                   (reductionCoefficient - 1.0) / (numberOfElementOnY - 1) * i;
    }
    else
    {
        alp = 1.0;
    }
    return alp;
}

/**
 * @brief Initialisation des variables globales
 */

void
MeshBuilder::initialize()
{
    // initialise la largeur de maille courante a (largeur totale)/(nbre de
    // mailles)
    dx = dimension.x / numberOfElementOnX;

    // initialise l'ordonnee courante a l'ordonnee de la base
    currentHeight = origin.y;
}

/**
 * @brief début du maillage (generation des noeuds de la base)
 */

void
MeshBuilder::meshFirstLine()
{
    for (int i = 0; i < numberOfElementOnX + 1; i++)
        target.nodes.push_back(
            new Point(origin.x + (double)i * dx, currentHeight));

    setContactNodes(0, numberOfElementOnX);
}

/**
 * @brief Maille la partie "gradient" ("nbm" couches)
 */

void
MeshBuilder::meshGradient()
{
    for (int lev = 0; lev < numberOfElementOnY; lev++)
        meshGradientLayer(lev);
}

double
MeshBuilder::getGradientDelta(int lev)
{
    double xp =
        (dimension.y - computeBoundaryHeight()) / computeReductionFactor();

    if (numberOfElementOnY > 1)
        return xp * (1.0 + (reductionCoefficient - 1.0) /
                               ((double)(numberOfElementOnY - 1)) *
                               (double)(numberOfElementOnY - 1 - lev));
    else
        return xp;
}

void
MeshBuilder::meshGradientLayer(int lev)
{
    increaseHeight(getGradientDelta(lev));

    addConstantNodes();
    addConstantElements();

    setContactNodes(last + 1, last + (last - first) + 1);
}

/**
 * @brief Maille la partie "boundary" ("type.size()" couches)
 */

void
MeshBuilder::meshBoundary()
{
    for (int lev = 0; lev < layers.size(); lev++)
    {
        switch (layers[lev])
        {
        case REDUCTION:
            meshReductionLayer();
            break;
        case CONSTANT:
            meshConstantLayer();
            break;
        }
    }
}

/**
 * @brief Maille une couche de "reduction"
 */

void
MeshBuilder::addReductionNodes()
{
    // Points intermediaires
    increaseHeight((target.nodes[first + 1]->x - target.nodes[first]->x) / 2.0);

    for (int i = first; i < last; i++)
        target.nodes.push_back(
            new Point((target.nodes[i]->x + target.nodes[i + 1]->x) / 2.0,
                      currentHeight));

    for (int i = first + 1; i < last; i += 2)
        target.nodes.push_back(new Point(target.nodes[i]->x, currentHeight));

    // Ajout des pts du niv. suivant
    increaseHeight((target.nodes[first + 1]->x - target.nodes[first]->x) / 2.0);

    double x = origin.x;
    target.nodes.push_back(new Point(x, currentHeight));

    dx /= 2.0;
    for (int i = first; i < last; i++)
    {
        x += dx;
        target.nodes.push_back(new Point(x, currentHeight));
        x += dx;
        target.nodes.push_back(new Point(x, currentHeight));
    }
}

void
MeshBuilder::addReductionElements()
{
    int nb = last - first;
    for (int i = 0; i < nb; i += 2)
    {
        int n[11];
        n[0] = first + i;
        n[1] = first + i + 1;
        n[2] = first + i + 2;
        n[3] = last + i + 1;
        n[4] = last + i + 2;
        n[5] = last + nb + nb / 2 + 2 * i + 1;
        n[6] = last + nb + nb / 2 + 2 * i + 2;
        n[7] = last + nb + nb / 2 + 2 * i + 3;
        n[8] = last + nb + nb / 2 + 2 * i + 4;
        n[9] = last + nb + nb / 2 + 2 * i + 5;
        n[10] = last + nb + i / 2 + 1;

        target.elements.push_back(new Element(n[0], n[3], n[6], n[5]));
        target.elements.push_back(new Element(n[0], n[1], n[10], n[3]));
        target.elements.push_back(new Element(n[10], n[7], n[6], n[3]));
        target.elements.push_back(new Element(n[1], n[2], n[4], n[10]));
        target.elements.push_back(new Element(n[10], n[4], n[8], n[7]));
        target.elements.push_back(new Element(n[4], n[2], n[9], n[8]));
    }
}

void
MeshBuilder::meshReductionLayer()
{
    addReductionNodes();
    addReductionElements();
    setContactNodes(target.nodes.size() - 1 - 2 * (last - first),
                    target.nodes.size() - 1);
}

/**
 * @brief Maille une couche "constante"
 */

void
MeshBuilder::addConstantNodes()
{
    for (int i = first; i < last + 1; i++)
    {
        Point *p = new Point(target.nodes[i]->x, currentHeight);
        target.nodes.push_back(p);
    }
}

void
MeshBuilder::addConstantElements()
{
    for (int i = 0; i < last - first; i++)
    {
        Element *e =
            new Element(first + i, first + i + 1, last + i + 2, last + i + 1);
        target.elements.push_back(e);
    }
}

void
MeshBuilder::meshConstantLayer()
{
    increaseHeight(target.nodes[first + 1]->x - target.nodes[first]->x);

    addConstantNodes();
    addConstantElements();
    setContactNodes(last + 1, last + (last - first) + 1);
}

/**
 * @brief génération du maillage a l'aide des parametres courants
 */

void
MeshBuilder::genere()
{
    if (!target.isEmpty())
        target.clear();

    initialize();

    meshFirstLine();
    meshGradient();
    meshBoundary();

    // Noeuds frontieres
    target.firstContactNode = first;
    target.lastContactNode = last;
}
