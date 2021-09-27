#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright 2020 University of Liège
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.




import os, sys, subprocess, platform

try:
    # create build dir
    os.chdir('..')
    if not os.path.isdir('louisB'): os.mkdir('louisB')
    os.chdir('louisB')
    # cmake
    if 'Windows' in platform.uname():
        subprocess.call(r'cmake -G "Visual Studio 11 Win64" ..\louis', shell=True)
    else:
        subprocess.call('cmake -G"Eclipse CDT4 - Unix Makefiles" -DCMAKE_ECLIPSE_VERSION=4.6 -DCMAKE_ECLIPSE_GENERATE_SOURCE_PROJECT=TRUE ../louis', shell=True)
        #subprocess.call('cmake -G"Eclipse CDT4 - Unix Makefiles" -DCMAKE_ECLIPSE_GENERATE_SOURCE_PROJECT=TRUE ../louis', shell=True)
    subprocess.call('cmake --build . --config Release', shell=True)
    os.chdir('../louis')
except Exception as e:
    print(e)
    print("<press ENTER to quit>"); input()
