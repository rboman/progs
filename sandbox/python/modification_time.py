#! /usr/bin/env python
# -*- coding: utf-8 -*-
# display the modification time of the modules loaded in memory
#
#   Copyright 2017 Romain Boman
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

import sys,os.path

print sys.modules.items()

for txt, m in sys.modules.items():
    try:
    #print txt, m
    #if 0:
        f = m.__file__
        if(os.path.isfile(f)):
            #if(f.find("python")==-1):
            mtime = os.path.getmtime(f)
            print txt, mtime
            mods[txt]=mtime
    except:
        pass
    
    
    

