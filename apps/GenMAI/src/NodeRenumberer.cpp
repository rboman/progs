#include "Global.h"
#include "NodeRenumberer.h"
#include "Mesh.h"

NodeRenumberer::NodeRenumberer(Mesh &mesh) : mesh(mesh), style(NORMALSTYLE)
{
}

void
NodeRenumberer::execute()
{
    switch (style)
    {
    case NORMALSTYLE:
        executeNormalStyle();
        break;
    case BACONSTYLE:
        executeBaconStyle();
        break;
    default:
        break;
    }
}

void 
NodeRenumberer::executeNormalStyle()
{
    for(auto i=0; i<mesh.numberOfNodes(); ++i)
        mesh.setNodeNumber(i,PtNumber(i+1));
}

void
NodeRenumberer::executeBaconStyle()
{
    int merde[10] = {  9999, 11111, 22222, 33333, 44444,
                      55555, 66666, 77777, 88888, 99999 };

    int k=0;
    int nbmerde = sizeof(merde)/sizeof(int);  // nbre de merdes
    int maxno   = mesh.numberOfNodes();      // numero max si pas de merde

    for(auto i=0; i<mesh.numberOfNodes(); ++i)
    {
        int no = i+1;
        if(no == merde[k])
        {
            k++;
            std::cout << "BaconMeshExporter: numero " << no << " converti en ";
            while(1)
            {
                maxno++;
                int j=0;
                for(j=k; j<nbmerde; ++j)
                    if(maxno==merde[j]) break;
                if(j==nbmerde) break;
            }
            no = maxno;
            std::cout << no << std::endl;
        }
        mesh.setNodeNumber(i, PtNumber(no));
    }
}

void 
NodeRenumberer::setStyle(RenumberStyle style) 
{ 
    this->style = style;
}

