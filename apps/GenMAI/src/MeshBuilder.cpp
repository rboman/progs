//   Copyright 2003-2017 Romain Boman
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

#include "MeshBuilder.h"
#include "Mesh.h"

MeshBuilder::MeshBuilder(Mesh &_target) : Builder(), target(_target)
{
}

/**
 * @brief Calcul la hauteur de la zone "boundary"
 */

double
MeshBuilder::computeBoundaryHeight()
{
    double dx2 = par.dimension.x / par.numberOfElementOnX;

    double hcl = dx2;
    int i;
    for (i = 1; i < par.layers.size(); i++)
    {
        if (par.layers[i - 1] == REDUCTION)
            dx2 /= 2.0;
        hcl += dx2;
    }

    // verification
    if (hcl > par.dimension.y)
    {
        printf("\nerreur: hauteur CL > hauteur totale\n");
        exit(1);
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

    if (par.numberOfElementOnY > 1)
    {
        int i;
        for (i = 0; i < par.numberOfElementOnY; i++)
            alp += 1.0 + (par.reductionCoefficient - 1.0) / (par.numberOfElementOnY - 1) * i;
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

void MeshBuilder::Initialize()
{
    // initialise la largeur de maille courante a (largeur totale)/(nbre de mailles)
    dx = par.dimension.x / par.numberOfElementOnX;

    // initialise l'ordonnee courante a l'ordonnee de la base
    currentHeight = par.origin.y;
}

/**
 * @brief début du maillage (generation des noeuds de la base)
 */

void MeshBuilder::meshFirstLine()
{
    int i;
    for (i = 0; i < par.numberOfElementOnX + 1; i++)
        target.addNode(par.origin.x + (double)i * dx, currentHeight);

    setContactNodes(0, par.numberOfElementOnX);
}

/**
 * @brief Maille la partie "gradient" ("par.nbm" couches)
 */

void MeshBuilder::meshGradient()
{
    int lev;
    for (lev = 0; lev < par.numberOfElementOnY; lev++)
        meshGradientLayer(lev);
}

double
MeshBuilder::getGradientDelta(int lev)
{
    double xp = (par.dimension.y - computeBoundaryHeight()) / computeReductionFactor();

    if (par.numberOfElementOnY > 1)
        return xp * (1.0 + (par.reductionCoefficient - 1.0) / ((double)(par.numberOfElementOnY - 1)) * (double)(par.numberOfElementOnY - 1 - lev));
    else
        return xp;
}

void MeshBuilder::meshGradientLayer(int lev)
{
    increaseHeight(getGradientDelta(lev));

    addConstantNodes();
    addConstantElements();

    setContactNodes(last + 1, last + (last - first) + 1);
}

/**
 * @brief Maille la partie "boundary" ("par.type.size()" couches)
 */

void MeshBuilder::meshBoundary()
{
    int lev;
    for (lev = 0; lev < par.layers.size(); lev++)
    {
        switch (par.layers[lev])
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

void MeshBuilder::addReductionNodes()
{
    // Points intermediaires
    increaseHeight((target.getNodeX(first + 1) - target.getNodeX(first)) / 2.0);

    int i;
    for (i = first; i < last; i++)
    {
        target.addNode((target.getNodeX(i) + target.getNodeX(i + 1)) / 2.0, currentHeight);
    }
    for (i = first + 1; i < last; i += 2)
    {
        target.addNode(target.getNodeX(i), currentHeight);
    }

    // Ajout des pts du niv. suivant
    increaseHeight((target.getNodeX(first + 1) - target.getNodeX(first)) / 2.0);

    double x = par.origin.x;
    target.addNode(x, currentHeight);

    dx /= 2.0;
    for (i = first; i < last; i++)
    {
        x += dx;
        target.addNode(x, currentHeight);
        x += dx;
        target.addNode(x, currentHeight);
    }
}

void MeshBuilder::addReductionElements()
{
    int nb = last - first;
    int i;
    for (i = 0; i < last - first; i += 2)
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
        target.addElement(n[0], n[3], n[6], n[5]);
        target.addElement(n[0], n[1], n[10], n[3]);
        target.addElement(n[10], n[7], n[6], n[3]);
        target.addElement(n[1], n[2], n[4], n[10]);
        target.addElement(n[10], n[4], n[8], n[7]);
        target.addElement(n[4], n[2], n[9], n[8]);
    }
}

void MeshBuilder::meshReductionLayer()
{
    addReductionNodes();
    addReductionElements();
    setContactNodes(target.numberOfNodes() - 1 - 2 * (last - first),
                    target.numberOfNodes() - 1);
}

/**
 * @brief Maille une couche "constante"
 */

void MeshBuilder::addConstantNodes()
{
    int i;
    for (i = first; i < last + 1; i++)
    {
        target.addNode(target.getNodeX(i), currentHeight);
    }
}

void MeshBuilder::addConstantElements()
{
    int i;
    for (i = 0; i < last - first; i++)
    {
        target.addElement(first + i,
                          first + i + 1,
                          last + i + 2,
                          last + i + 1);
    }
}

void MeshBuilder::meshConstantLayer()
{
    increaseHeight(target.getNodeX(first + 1) - target.getNodeX(first));

    addConstantNodes();
    addConstantElements();
    setContactNodes(last + 1, last + (last - first) + 1);
}

/**
 * @brief génération du maillage a l'aide des parametres courants
 */

void MeshBuilder::genere()
{
    if (!target.isEmpty())
        target.clear();

    Initialize();

    meshFirstLine();
    meshGradient();
    meshBoundary();

    // Noeuds frontieres
    target.setFirstContactNode(first);
    target.setLastContactNode(last);
}

/**
 * @brief Set the parameters of the builder (copy the given object)
 */

void MeshBuilder::setParameters(const MeshParameters &p)
{
    par = p;
}

/**
 * @brief Prints the parameters to stdout
 */

void MeshBuilder::printParameters() const
{
    //par.print();
}
