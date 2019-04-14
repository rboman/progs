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

#ifndef NODERENUMBERER_H
#define NODERENUMBERER_H

#include "genmai.h"

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

class GENMAI_API NodeRenumberer
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
