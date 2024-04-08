# init sph module

# needed here because multiprocessing.Pool does not find the DLLs
import platform, os, sys
if platform.system() == 'Windows' and sys.version_info.minor >= 8:
    # required for Python 3.8 (msvc) on windows
    # (MSYS2 executes this, even it is not required)
    for v in os.environ['path'].split(';'):
        if os.path.exists(v):
            os.add_dll_directory(v)

from sphw import *
from .utils import *
