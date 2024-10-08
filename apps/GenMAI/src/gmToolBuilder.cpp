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

#include "gmToolBuilder.h"
#include "gmPolarPoint.h"
#include "gmTool.h"
#include "gmCurve.h"

using namespace genmai;

genmai::Point const &
ToolBuilder::getRollAxis() const
{
    static const Point rollAxis(0, -1);
    return rollAxis;
}

ToolBuilder::ToolBuilder(Tool &_target) : Object(), target(_target)
{
    // default pars
    radius = 100.0;
    initialAngle = 0.050;
    asperityLength = 10.0;
    asperityAngle = 40.0;
    smoothnessAngle = 1.0;
    asperityInterval = 5.0;
    numberOfAsperities = 10;
    centre.x = 10.0;
    centre.y = 110.0;
}

void
ToolBuilder::write(std::ostream &out) const
{
    out << "ToolBuilder:\n";
    out << "\tradius             : " << radius << '\n';
    out << "\tinitialAngle       : " << initialAngle << '\n';
    out << "\tasperityLength     : " << asperityLength << '\n';
    out << "\tasperityAngle      : " << asperityAngle << '\n';
    out << "\tsmoothnessAngle    : " << smoothnessAngle << '\n';
    out << "\tasperityInterval   : " << asperityInterval << '\n';
    out << "\tnumberOfAsperities : " << numberOfAsperities << '\n';
    out << "\tcentre     : " << centre << '\n';
}

/**
 * @brief génération de la matrice a l'aide des parametres courants
 */

void
ToolBuilder::genere()
{
    std::cout << "ToolBuilder::genere()...\n";

    // Vide le maillage s'il existe
    if (!target.isEmpty())
        target.clear();

    // generation 1er point */
    target.points.push_back(
        new Point(centre, getRollAxis(), d2r(initialAngle), radius));

    // generation des aspérites
    for (size_t i = 0; i < numberOfAsperities; i++)
    {
        genereAsperity();
        if (asperityInterval > 0.0)
            genereInterval();
    }

    // a partir d'ici, on va reconsidérer tous les
    // noeuds 3 par 3 pour créer des congés de
    // raccordement. Les points 1->nbpt2 et les courbes
    // 1->nbcou2 ne seront pas sauvées

    // Generation des conges
    // ---------------------

    const size_t nbpt2 = target.points.size();
    const size_t nbcou2 = target.curves.size();

    // On copie le premier point

    target.points.push_back(new Point(*target.points[0]));

    // Boucle sur les points 3 par 3

    size_t np0 = nbpt2 + 1;
    size_t np1 = nbpt2 + 1;

    for (size_t i = 1; i < nbpt2 - 1; i++)
    {
        genereSmoothMatrix(np0, &np1, i);
        np0 = np1;
    }

    // Creation du dernier point
    target.points.push_back(new Point(*target.points[nbpt2 - 1]));

    // Création de la derniere ligne
    target.curves.push_back(
        new Curve({target.points.size() - 1, target.points.size()}));

    target.firstp = nbpt2;
    target.firstc = nbcou2;
}

double
ToolBuilder::d2r(double angle)
{
    return (angle / 180.0 * M_PI);
}

double
ToolBuilder::r2d(double angle)
{
    return (angle * 180.0 / M_PI);
}

void
ToolBuilder::genereAsperity()
{
    // index courant - récupère le dernier point
    size_t pr = target.points.size() - 1;
    const Point &p1 = *target.points[pr];

    // point 1
    PolarPoint pp1(centre, getRollAxis(), p1);

    // point 2
    Point p2(centre, getRollAxis(), pp1.a - asperityLength / radius, radius);

    // point 3
    Point tmp(p2 - p1);
    Point p3(p1, tmp, d2r(asperityAngle),
             tmp.length() / 2.0 / cos(d2r(asperityAngle)));

    // ajout des points nouvellement crees
    target.points.push_back(new Point(p3)); // pr+2
    target.points.push_back(new Point(p2)); // pr+3

    // ajoute les courbes
    target.curves.push_back(new Curve({pr + 1, pr + 2}));
    target.curves.push_back(new Curve({pr + 2, pr + 3}));
}

void
ToolBuilder::genereInterval()
{
    size_t pr = target.points.size() - 1;
    const Point &p1 = *target.points[pr];

    PolarPoint pp1(centre, getRollAxis(), p1);
    Point p2(centre, getRollAxis(), pp1.a - asperityInterval / radius, radius);

    target.points.push_back(new Point(p2));
    target.curves.push_back(new Curve({pr + 1, pr + 2}));
}

void
ToolBuilder::genereSmoothMatrix(size_t np0, size_t *np1, size_t i)
{
    const Point &p1 = *target.points[i - 1];
    const Point &p2 = *target.points[i + 1];
    const Point &p3 = *target.points[i];

    // index courant
    size_t pr = target.points.size() - 1;

    // calcul de l'angle 1,2 - 1,3

    Point dx1(p3 - p1);
    Point dx2(p2 - p1);
    Point dx3(p2 - p3);

    int signpv = ((dx1 ^ dx2) >= 0) ? 1 : -1;

    double angle1 = acos((dx1 * dx2) / (dx1.length() * dx2.length()));
    double angle2 = acos((dx2 * dx3) / (dx3.length() * dx2.length()));

    Point p4(p1, p3,
             smoothnessAngle / tan((M_PI - angle1 - angle2) / 2.0) /
                 dx1.length());
    Point p5(p2, p3,
             smoothnessAngle / tan((M_PI - angle1 - angle2) / 2.0) /
                 dx3.length());
    Point p6(p4, (p3 - p1), signpv * (M_PI / 2.0), smoothnessAngle);
    Point p7(p6, (p4 - p6), signpv * (angle1 + angle2) / 2.0, smoothnessAngle);

    // ajout des points nouvellement crees
    target.points.push_back(new Point(p4)); // pr+2
    target.points.push_back(new Point(p7)); // pr+3
    target.points.push_back(new Point(p5)); // pr+4

    // ajoute les courbes
    target.curves.push_back(new Curve({np0, pr + 2}));
    target.curves.push_back(new Curve({pr + 2, pr + 3, pr + 4}));

    *np1 = pr + 4;
}
