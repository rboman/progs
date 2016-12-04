//
// $Id$
//

#include "Global.h"
#include "MeshBuilder.h"
#include "Mesh.h"

/**
 * @brief Constructeur
 */

MeshBuilder::MeshBuilder(Mesh &_target) : Builder(), target(_target)
{
}

/**
 * @brief Calcul la hauteur de la zone "boundary"
 */

double
MeshBuilder::computeBoundaryHeight()
{
    double dx2 = par.getDimension().getX()/par.getNumberOfElementOnX();

    double hcl = dx2;
    int i;
    for(i=1; i<par.getNumberOfLayers(); i++)
    {
        if(par.getLayerType(i-1)==REDUCTION) 
            dx2/=2.0;
        hcl+=dx2;
    }

    // verification
    if(hcl>par.getDimension().getY()) 
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

    if(par.getNumberOfElementOnY()>1)
    {
        int i;
        for(i=0; i<par.getNumberOfElementOnY(); i++)
            alp += 1.0 + (par.getReductionCoefficient()-1.0)/(par.getNumberOfElementOnY()-1) * i;
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
MeshBuilder::Initialize()
{
    // initialise la largeur de maille courante a (largeur totale)/(nbre de mailles)
    dx = par.getDimension().getX()/par.getNumberOfElementOnX();

    // initialise l'ordonnee courante a l'ordonnee de la base
    currentHeight  = par.getOrigin().getY();
}


/**
 * @brief début du maillage (generation des noeuds de la base)
 */

void 
MeshBuilder::meshFirstLine()
{
    int i;
    for(i=0; i<par.getNumberOfElementOnX()+1; i++)
        target.addNode(par.getOrigin().getX() + (double)i*dx, currentHeight);

    setContactNodes(0, par.getNumberOfElementOnX());
}

/**
 * @brief Maille la partie "gradient" ("par.nbm" couches)
 */

void 
MeshBuilder::meshGradient()
{
    int lev;
    for(lev=0; lev<par.getNumberOfElementOnY(); lev++) 
        meshGradientLayer(lev);
}

double 
MeshBuilder::getGradientDelta(int lev)
{
    double xp  = (par.getDimension().getY()-computeBoundaryHeight())/computeReductionFactor();

    if(par.getNumberOfElementOnY()>1)
        return xp * (1.0 + (par.getReductionCoefficient()-1.0)/((double)(par.getNumberOfElementOnY()-1))*(double)(par.getNumberOfElementOnY()-1-lev));
    else
        return xp;
}


void 
MeshBuilder::meshGradientLayer(int lev)
{
    increaseHeight( getGradientDelta(lev) );    

    addConstantNodes();
    addConstantElements();

    setContactNodes(last+1, last+ (last-first)+ 1);
}

/**
 * @brief Maille la partie "boundary" ("par.type.size()" couches)
 */

void 
MeshBuilder::meshBoundary()
{
    int lev;
    for(lev=0; lev<par.getNumberOfLayers(); lev++) 
    {
        switch(par.getLayerType(lev))
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
    increaseHeight( ( target.getNodeX(first+1) - target.getNodeX(first) )/2.0 );

    int i;
    for(i=first; i<last; i++)  
    {
        target.addNode( ( target.getNodeX(i) + target.getNodeX(i+1) )/2.0, currentHeight);
    }
    for(i=first+1;i<last;i+=2) 
    {
        target.addNode(target.getNodeX(i), currentHeight);
    }

    // Ajout des pts du niv. suivant
    increaseHeight( ( target.getNodeX(first+1) - target.getNodeX(first) )/2.0 );    

    double x = par.getOrigin().getX();
    target.addNode(x,currentHeight);

    dx /= 2.0;
    for(i=first; i<last; i++) 
    {
        x+=dx;
        target.addNode(x,currentHeight);
        x+=dx;
        target.addNode(x,currentHeight);
    }

}

void 
MeshBuilder::addReductionElements()
{
    int nb=last-first;
    int i;
    for(i=0; i<last-first; i+=2) 
    {
        IntNumber n[11];
        n[0] = IntNumber(first+i);  
        n[1] = IntNumber(first+i+1); 
        n[2] = IntNumber(first+i+2);
        n[3] = IntNumber(last+i+1); 
        n[4] = IntNumber(last+i+2);
        n[5] = IntNumber(last+nb+nb/2+2*i+1); 
        n[6] = IntNumber(last+nb+nb/2+2*i+2);
        n[7] = IntNumber(last+nb+nb/2+2*i+3); 
        n[8] = IntNumber(last+nb+nb/2+2*i+4);
        n[9] = IntNumber(last+nb+nb/2+2*i+5);
        n[10]= IntNumber(last+nb+i/2+1);
        target.addElement(n[0], n[3], n[6], n[5]);  
        target.addElement(n[0], n[1], n[10],n[3]);
        target.addElement(n[10],n[7], n[6], n[3]); 
        target.addElement(n[1], n[2], n[4], n[10]);
        target.addElement(n[10],n[4], n[8], n[7]); 
        target.addElement(n[4], n[2], n[9], n[8]);
    }
}

void 
MeshBuilder::meshReductionLayer()
{
    addReductionNodes();
    addReductionElements();
    setContactNodes(target.numberOfNodes()-1 - 2*(last-first), 
                    target.numberOfNodes()-1);
}

/**
 * @brief Maille une couche "constante"
 */

void 
MeshBuilder::addConstantNodes()
{
    int i;
    for(i=first; i<last+1; i++) 
    {
        target.addNode(target.getNodeX(i),currentHeight);
    }
}

void 
MeshBuilder::addConstantElements()
{
    int i;
    for(i=0; i<last - first; i++) 
    {
        target.addElement(IntNumber(first+i), 
                          IntNumber(first+i+1), 
                          IntNumber(last+i+2), 
                          IntNumber(last+i+1) );
    }
}

void 
MeshBuilder::meshConstantLayer()
{
    increaseHeight( target.getNodeX(first+1) - target.getNodeX(first) );    

    addConstantNodes();
    addConstantElements();
    setContactNodes(last+1, last+ (last-first)+ 1);
}

/**
 * @brief génération du maillage a l'aide des parametres courants
 */

void
MeshBuilder::genere()
{
    if(!target.isEmpty()) target.clear();

    Initialize();

    meshFirstLine();
    meshGradient();
    meshBoundary();

    // Noeuds frontieres
    target.setFirstContactNode( first );
    target.setLastContactNode( last );
}

/**
 * @brief Set the parameters of the builder (copy the given object)
 */

void
MeshBuilder::setParameters(const MeshParameters &p)
{
    par = p;
}

/**
 * @brief Prints the parameters to stdout
 */

void
MeshBuilder::printParameters() const
{
    par.print();
}
