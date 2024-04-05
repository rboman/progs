import platform, os, sys
if platform.system() == 'Windows' and sys.version_info.minor >= 8:
    for v in os.environ['path'].split(';'):
        if os.path.exists(v):
            os.add_dll_directory(v)

from sphw import *

