#! /usr/bin/env python3
# -*- coding: utf-8 -*-
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

# copy dependencies (.so files) of "exefile" to "targetPath"

def cplibs(exefile, targetPath='lddLibs', libPaths=[]):
    """
    copy dependencies (.so files) of "exefile" to "targetPath"
    unless the .so is found in "libPaths"
    expl: cplibs('Metafor', 'lddLibs', ['libs', 'python/lib'])
    """
    import subprocess, os, os.path, re, shutil
    if not os.path.isdir(targetPath):
        print("creating target folder (%s)" %targetPath)
        os.mkdir(targetPath)
    
    libPaths.append(targetPath)
    
    # check if file is there
    if not os.path.isfile(exefile):
        raise Exception("'%s' not found" % exefile)
     
    # add basedir to LD_LIBRARY_PATH  
    basedir=os.path.abspath(os.path.dirname(exefile))
    print("adding '%s' to LD_LIBRARY_PATH" % basedir)
    try:
        ldpath=os.environ['LD_LIBRARY_PATH']+':'+basedir
    except:
        ldpath=basedir
    os.environ['LD_LIBRARY_PATH'] = ldpath
    out = subprocess.check_output(["ldd", exefile])
    
    libs=[]
    reg1=r"^([^=]+)=>([^(]+)"
    exp1 = re.compile(reg1)
    for line in out.split('\n'):
        m = exp1.match(line)
        if m and m.group(0):
            lib=m.group(1).strip(' \t')  # library
            libf=m.group(2).strip(' \t') # full path
            if not libf:
                continue
            # check if library already exists in libPaths

            for d in libPaths:
                f=os.path.join(d,lib)
                if os.path.isfile(f):
                    print("'%s' already in '%s'" % (lib, d))
                    break
            else:
                print("copying '%s' to '%s'" % (lib, targetPath))
                shutil.copy(libf, d)
                
if __name__=="__main__":
    import sys
    if len(sys.argv)<3:
        print("\nusage: %s [exefile] [target_folder]\n" % sys.argv[0])
    else:
        cplibs(sys.argv[1], sys.argv[2])
        
