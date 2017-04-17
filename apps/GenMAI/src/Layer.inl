//
// $Id: Layer.inl,v 1.2 2003/07/07 14:48:38 Boman Exp $
//

inline
Layer::Layer(const LayerType type, const std::string &name) : type(type), name(name)
{
}

inline LayerType   
Layer::getType() const
{
    return type;
}

inline std::string const &
Layer::getName() const
{
    return name;
}

inline bool 
Layer::operator!=(const Layer &l) const
{
    return type!=l.type;
}

inline bool 
Layer::operator==(const Layer &l) const
{
    return type==l.type;
}

inline void
Layer::operator=(const Layer &l)
{
    name = l.name;
    type = l.type;
}


