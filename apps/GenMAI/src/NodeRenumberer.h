#ifndef NODERENUMBERER_H
#define NODERENUMBERER_H

class Mesh;

enum RenumberStyle 
{
    NORMALSTYLE = 0,
    BACONSTYLE
};

/**
 *  @brief Renumbers the nodes from a Mesh in the normal or the Bacon style (for which some
 *         numbers are forbidden)
 */

class NodeRenumberer
{
    RenumberStyle style;
    Mesh &mesh; 

public:
    NodeRenumberer(Mesh &mesh);
    void setStyle(RenumberStyle style);
    void execute();

private:
    void executeBaconStyle();
    void executeNormalStyle();
};

#endif
