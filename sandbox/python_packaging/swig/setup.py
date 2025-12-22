from setuptools import setup, Extension
from setuptools.command.build_ext import build_ext

mymath_extension = Extension(
    name="mon_projet_swig.mymath",
    sources=[
        "src/mon_projet_swig/mymath.i",
        "src/mon_projet_swig/mymath_impl.cpp",
    ],
    swig_opts=[
        "-c++",
        "-py3",
        "-builtin",
        "-O",
        "-Isrc/mon_projet_swig",
    ],
    include_dirs=["src/mon_projet_swig"],
    language="c++",
)

setup(ext_modules=[mymath_extension])
