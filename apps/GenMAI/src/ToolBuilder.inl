//
// $Id: ToolBuilder.inl,v 1.2 2003/07/07 15:03:22 Boman Exp $
//

inline void 
ToolBuilder::addPoint(const Point &arg)
{
    target.addPoint(arg);
}

inline void 
ToolBuilder::addCurve(Curve *arg)
{
    target.addCurve(arg);
}
