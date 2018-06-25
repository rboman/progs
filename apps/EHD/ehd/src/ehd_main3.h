/*
 *   Copyright 2000-2017 Romain Boman
 *
 *   Licensed under the Apache License, Version 2.0 (the "License");
 *   you may not use this file except in compliance with the License.
 *   You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

#ifndef __EHD_MAIN3_H__
#define __EHD_MAIN3_H__

#include "ehd.h"


class EHD_API Main3
{
    int nn;
public:
    Main3(int _nn=100);

    int execute();

    std::vector<double> h;
    std::vector<double> PhiS;
    std::vector<double> PhiP;
    std::vector<double> dPhiS;
    std::vector<double> dPhiP;
};

#endif // __EHD_MAIN3_H__