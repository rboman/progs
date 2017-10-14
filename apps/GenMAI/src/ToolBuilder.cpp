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

#include "Global.h"
#include "ToolBuilder.h"
#include "Point.h"
#include "PolarPoint.h"
#include "Arc.h"
#include "Line.h"
#include <math.h>

double ToolBuilder::pi = 4.0*atan(1.0);

Point const & 
ToolBuilder::getRollAxis() const
{
    static Point rollAxis(0,-1);
    return rollAxis;
}

ToolBuilder::ToolBuilder(Tool &_target) : Builder(), target(_target)
{
}

/**
 * @brief génération de la matrice a l'aide des parametres courants
 */

void
ToolBuilder::genere()
{
    std::cout << "ToolBuilder::genere()...\n";

    // Vide le maillage s'il existe
    if(!target.isEmpty()) target.clear();

    // generation 1er point */
    addPoint(Point(par.getCentre(),
                     getRollAxis(), 
                     d2r(par.getInitialAngle()), 
                     par.getRadius()));
    
    // generation des aspérites
    for(size_t i=0; i<par.getNumberOfAsperities(); i++)
    {
        genereAsperity();
        if(par.getAsperityInterval()>0.0) 
            genereInterval();
    }

    // a partir d'ici, on va reconsidérer tous les
    // noeuds 3 par 3 pour créer des congés de
    // raccordement. Les points 1->nbpt2 et les courbes
    // 1->nbcou2 ne seront pas sauvées
    
    // Generation des conges
    // ---------------------

    const size_t nbpt2  = target.numberOfPoints();
    const size_t nbcou2 = target.numberOfCurves();
    
    // On copie le premier point
    
    addPoint(target.getPoint(0));
    
    // Boucle sur les points 3 par 3
    
    size_t np0 = nbpt2+1; 
    size_t np1 = nbpt2+1;

    for(size_t i=1; i<nbpt2-1; i++)
    {
        genereSmoothMatrix(np0, &np1, i);
        np0 = np1;
    }
    
    // Creation du dernier point

    addPoint(target.getPoint(nbpt2-1));
    
    // Création de la derniere ligne
    addCurve(new Line(target.numberOfPoints()-1, target.numberOfPoints()));

    target.setFirstPoint( nbpt2 );
    target.setFirstCurve( nbcou2 );
}


double
ToolBuilder::d2r(double angle)
{
    return(angle/180.0*pi);
}

double 
ToolBuilder::r2d(double angle)
{
    return(angle*180.0/pi);
}

void
ToolBuilder::genereAsperity()
{
    const Point &p1 = target.getPoint(target.numberOfPoints()-1);

    // index courant
    size_t pr = target.numberOfPoints()-1;
    
    // point 1
    PolarPoint pp1(par.getCentre(), getRollAxis(), p1);
    
    // point 2
    Point p2(par.getCentre(), 
             getRollAxis(), 
             pp1.getA()-par.getAsperityLength()/par.getRadius(), 
             par.getRadius());
    
    // point 3
    Point tmp(p2-p1);
    Point p3(p1, tmp, 
             d2r(par.getAsperityAngle()), 
             tmp.length()/2.0/cos(d2r(par.getAsperityAngle())));
    
    // ajout des points nouvellement crees
    addPoint(p3); // pr+2
    addPoint(p2); // pr+3
    
    // ajoute les courbes
    addCurve(new Line(pr+1,pr+2));
    addCurve(new Line(pr+2,pr+3));
}

void
ToolBuilder::genereInterval()
{
    const Point &p1 = target.getPoint(target.numberOfPoints()-1);
    size_t pr = target.numberOfPoints()-1;

    PolarPoint pp1(par.getCentre(), getRollAxis(), p1);
    Point p2(par.getCentre(), 
             getRollAxis(), 
             pp1.getA()-par.getAsperityInterval()/par.getRadius(), 
             par.getRadius());
    
    addPoint(p2); 
    addCurve(new Line(pr+1,pr+2));
}

void
ToolBuilder::genereSmoothMatrix(size_t np0, size_t *np1, size_t i)
{
    const Point &p1 = target.getPoint(i-1);
    const Point &p2 = target.getPoint(i+1);
    const Point &p3 = target.getPoint(i);

    // index courant
    size_t pr = target.numberOfPoints()-1;
    
    // calcul de l'angle 1,2 - 1,3

    Point dx1(p3-p1);
    Point dx2(p2-p1);
    Point dx3(p2-p3);

    int signpv = ((dx1^dx2)>=0)? 1 : -1;

    double angle1 = acos((dx1*dx2)/(dx1.length()*dx2.length()));
    double angle2 = acos((dx2*dx3)/(dx3.length()*dx2.length()));

    Point p4(p1, p3,      par.getSmoothnessAngle()/tan((pi-angle1-angle2)/2.0)/dx1.length());
    Point p5(p2, p3,      par.getSmoothnessAngle()/tan((pi-angle1-angle2)/2.0)/dx3.length());
    Point p6(p4, (p3-p1), signpv*(pi/2.0),            par.getSmoothnessAngle());
    Point p7(p6, (p4-p6), signpv*(angle1+angle2)/2.0, par.getSmoothnessAngle());

    // ajout des points nouvellement crees 
    addPoint(p4); // pr+2
    addPoint(p7); // pr+3
    addPoint(p5); // pr+4

    // ajoute les courbes
    addCurve(new Line(np0,pr+2));
    addCurve(new Arc(pr+2,pr+3,pr+4));
 
    *np1 = pr+4;
}

void
ToolBuilder::setParameters(const ToolParameters &p)
{
    par = p;
}

void
ToolBuilder::printParameters() const
{
    par.print();
}
