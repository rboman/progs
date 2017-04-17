//
// $Id: ToolParameters.inl,v 1.5 2003/07/07 18:17:13 Boman Exp $
//

inline double  
ToolParameters::getRadius() const
{
    return get(P_R).getDouble();
}

inline void 
ToolParameters::setRadius(double arg)
{
    get(P_R).set(arg);
}

inline double  
ToolParameters::getInitialAngle() const
{
    return get(P_A1).getDouble();
}

inline void 
ToolParameters::setInitialAngle(double arg)
{
    get(P_A1).set(arg);
}

inline double  
ToolParameters::getAsperityLength() const
{
    return get(P_ASP_B).getDouble();
}

inline void 
ToolParameters::setAsperityLength(double arg)
{
    get(P_ASP_B).set(arg);
}

inline double  
ToolParameters::getAsperityAngle() const
{
    return get(P_ASP_A).getDouble();
}

inline void 
ToolParameters::setAsperityAngle(double arg)
{
    get(P_ASP_A).set(arg);
}

inline double  
ToolParameters::getSmoothnessAngle() const
{
    return get(P_ASP_CR).getDouble();
}

inline void 
ToolParameters::setSmoothnessAngle(double arg)
{
    get(P_ASP_CR).set(arg);
}

inline double  
ToolParameters::getAsperityInterval() const
{
    return get(P_ABS_INT).getDouble();
}

inline void 
ToolParameters::setAsperityInterval(double arg)
{
    get(P_ABS_INT).set(arg);
}

inline Point const & 
ToolParameters::getCentre() const
{
    return get(P_CENTRE).getPoint();
}

inline void 
ToolParameters::setCentre(const Point &arg)
{
    get(P_CENTRE).set(arg);
}

inline void 
ToolParameters::setCentreX(double arg)
{
    get(P_CENTRE).getPoint().setX(arg);
}

inline void 
ToolParameters::setCentreY(double arg)
{
    get(P_CENTRE).getPoint().setY(arg);
}

inline int  
ToolParameters::getNumberOfAsperities() const
{
    return get(P_ASP_NBR).getInt();
}

inline void 
ToolParameters::setNumberOfAsperities(int arg)
{
    get(P_ASP_NBR).set(arg);
}

