
import wutils

import sys,os.path

for txt,m in sys.modules.items():
    try:
        f = m.__file__
        if(os.path.isfile(f)):
            if(f.find("python")==-1):
                mtime = os.path.getmtime(f)
                print txt,mtime
                mods[txt]=mtime
    except:
        pass
    
    
    

