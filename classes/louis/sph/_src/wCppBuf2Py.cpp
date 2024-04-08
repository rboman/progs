/*
 * Copyright 2020 University of Liège
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <Python.h>
#include "wCppBuf2Py.h"
#include <stdio.h>
#include <algorithm>

CppBuf2Py::CppBuf2Py(std::ostream &o1, bool err) : stream(o1), errstream(err)
{
    oldbuf = stream.rdbuf();
    stream.rdbuf(this);
    verb = false;
}

CppBuf2Py::~CppBuf2Py()
{
    stream.rdbuf(oldbuf); // restore the ostream
}

std::streamsize
CppBuf2Py::xsputn(const char *s, std::streamsize n)
{
    if (verb)
        printf("CppBuf2Py::xsputn(%ld)\n", static_cast<long>(n));
    // PySys_WriteStdout truncates to 1000 chars
    static const std::streamsize MAXSIZE = 1000;
    std::streamsize written = std::min(n, MAXSIZE);
    if (verb)
        printf("written=%ld\n", static_cast<long>(written));
    std::string str(s, n);
    if (verb)
        printf("string=%s\n", str.c_str());

    PyGILState_STATE gstate = PyGILState_Ensure();
    if (errstream)
        PySys_WriteStderr("%s", str.c_str());
    else
        PySys_WriteStdout("%s", str.c_str());
    PyGILState_Release(gstate);

    return written;
}

int CppBuf2Py::overflow(int c)
{
    if (verb)
        printf("CppBuf2Py::overflow(%d)\n", c);

    if (c != EOF)
    {
        PyGILState_STATE gstate = PyGILState_Ensure();
        if (errstream)
            PySys_WriteStderr("%c", c);
        else
            PySys_WriteStdout("%c", c);
        PyGILState_Release(gstate);
    }
    return c;
}

void CppBuf2Py::test()
{
    CppBuf2Py buf(std::cout);
    for (int i = 0; i < 1000; ++i)
        std::cout << "(" << i << ")"
                  << " this is a test - ";
}

// =============================================================================

StdOutErr2Py::StdOutErr2Py() : out(std::cout), err(std::cerr, true)
{
}

void StdOutErr2Py::test()
{
    StdOutErr2Py redirector;

    for (int i = 0; i < 50; ++i)
    {
        std::cout << "(" << i << ")"
                  << " this is a test - ";
        std::cerr << "(" << i << ")"
                  << " error - ";
    }
    std::cout << '\n';
    std::cerr << '\n';
}
