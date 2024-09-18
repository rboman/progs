#!/usr/bin/env python3

from distutils.core import setup, Extension

example_module = Extension('_example', sources = [ 'example_wrap.cxx', 'example.cpp' ])

setup(name = 'example',
      version = '0.1',
      author = 'SWIG Docs',
      description = """Simple swig example from docs""",
      ext_modules = [example_module],
      py_modules = ["example"],
      )
