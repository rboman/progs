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

/*
 * Test de la determination de h(x)
 */

#include "ehd.h"
#include <stdexcept>
#include <iostream>

int main()
{
    try
    {
        return ehd_main2();
    }
    catch(std::exception &e) 
    {
        std::cerr << "\n** ERROR:" << e.what() << '\n';
        return 1;
    }
    catch(...) 
    {
        std::cerr << "\n** ERROR: Unknown C++ Runtime Error\n";
        return 1;
    } 
}
