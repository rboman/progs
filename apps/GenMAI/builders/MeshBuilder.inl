//
// $Id: MeshBuilder.inl,v 1.2 2003/07/07 15:03:22 Boman Exp $
//

inline void
MeshBuilder::setContactNodes(int first, int last)
{
    this->first = first;
    this->last  = last;
}

inline void
MeshBuilder::increaseHeight(double dh)
{
    currentHeight+=dh;
}
