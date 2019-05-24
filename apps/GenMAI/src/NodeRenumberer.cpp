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

#include "NodeRenumberer.h"
#include "Mesh.h"

NodeRenumberer::NodeRenumberer(Mesh &mesh) : mesh(mesh), style(NORMALSTYLE)
{
}

void NodeRenumberer::execute()
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

void NodeRenumberer::executeNormalStyle()
{
    for (auto i = 0; i < mesh.numberOfNodes(); ++i)
        mesh.setNodeNumber(i, PtNumber(i + 1));
}

void NodeRenumberer::executeBaconStyle()
{
    int merde[10] = {9999, 11111, 22222, 33333, 44444,
                     55555, 66666, 77777, 88888, 99999};

    int k = 0;
    int nbmerde = sizeof(merde) / sizeof(int); // nbre de merdes
    int maxno = mesh.numberOfNodes();          // numero max si pas de merde

    for (auto i = 0; i < mesh.numberOfNodes(); ++i)
    {
        int no = i + 1;
        if (no == merde[k])
        {
            k++;
            std::cout << "BaconMeshExporter: numero " << no << " converti en ";
            while (1)
            {
                maxno++;
                int j = 0;
                for (j = k; j < nbmerde; ++j)
                    if (maxno == merde[j])
                        break;
                if (j == nbmerde)
                    break;
            }
            no = maxno;
            std::cout << no << std::endl;
        }
        mesh.setNodeNumber(i, PtNumber(no));
    }
}

void NodeRenumberer::setStyle(RenumberStyle style)
{
    this->style = style;
}
