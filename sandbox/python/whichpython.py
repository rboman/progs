import subprocess
import sys
import re

pyexe=sys.executable

out = subprocess.check_output([pyexe, '-c', 
    'from __future__ import print_function; import sys; print(sys.version_info.major)'])
try:
    pyver = int(out)
except:
    print('unable to get python version number')
print('Python version is', pyver)
