
# python setup.py build_ext -i

from distutils.core import setup, Extension
import mpi4py

swig = mpi4py.get_include()
include = [swig, "/usr/lib/openmpi/include"]
sources = ["testSwig.c","testSwig.i"]

setup( ext_modules = [
        Extension("_testSwig", sources = sources,
                  swig_opts = ["-I"+swig ],
                  include_dirs = include
                 )   ]
     )
     
     
