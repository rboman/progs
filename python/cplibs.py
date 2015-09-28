#!/usr/bin/env python
# copy dependencies (.so files) of "exefile" to "targetPath"

def cplibs(exefile, targetPath='lddLibs', libPaths=[]):
    """
    copy dependencies (.so files) of "exefile" to "targetPath"
    unless the .so is found in "libPaths"
    expl: cplibs('Metafor', 'lddLibs', ['libs', 'python/lib'])
    """
    import subprocess, os, os.path, re, shutil
    if not os.path.isdir(targetPath):
        print "creating target folder (%s)" %targetPath
        os.mkdir(targetPath)
    
    libPaths.append(targetPath)
    
    # check if file is there
    if not os.path.isfile(exefile):
        raise Exception("'%s' not found" % exefile)
     
    # add basedir to LD_LIBRARY_PATH  
    basedir=os.path.abspath(os.path.dirname(exefile))
    print "adding '%s' to LD_LIBRARY_PATH" % basedir
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
                    print "'%s' already in '%s'" % (lib, d)
                    break
            else:
                print "copying '%s' to '%s'" % (lib, targetPath)
                shutil.copy(libf, d)
                
if __name__=="__main__":
    import sys
    if len(sys.argv)<3:
        print "\nusage: %s [exefile] [target_folder]\n" % sys.argv[0]
    else:
        cplibs(sys.argv[1], sys.argv[2])
        
