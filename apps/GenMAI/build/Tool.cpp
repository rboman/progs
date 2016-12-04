//
// $Id$
//

#include "Global.h"
#include "Tool.h"
#include "curve.h"

/**
 * @brief Constructor
 */

Tool::Tool() : TargetObject(), point(0), courbe(0)
{
    clear();
}

void
Tool::addPoint(const Point &pt)
{
    point.push_back(pt);
}

void
Tool::addCurve(Curve *m)
{
    courbe.push_back(m);
}

void
Tool::addPoint(double x, double y)
{
    Point pt(x,y);
    addPoint(pt);
}

void
Tool::print() const
{
    std::cout << "MATRICE:" << std::endl;
    std::cout << "--------" << std::endl;
    std::cout << " points     : " << numberOfPoints()  << std::endl;
    std::cout << " courbes    : " << numberOfCurves() << std::endl;
    std::cout << std::endl;
}

void
Tool::list() const
{
    int i;
    for(i=0; i<numberOfPoints(); ++i)
        std::cout << point[i] << std::endl;

    for(i=0; i<numberOfCurves(); ++i)
    {
        courbe[i]->print();
        std::cout << std::endl;
    }
}

bool 
Tool::isEmpty() const
{ 
    if(!point.size()) 
        return true; 
    else 
        return false; 
}

void 
Tool::clear() 
{
    firstp = 0;
    firstc = 0;

    point.resize(0);

    int i;
    for(i=0; i<numberOfCurves(); ++i) delete courbe[i];
    courbe.resize(0);
}

