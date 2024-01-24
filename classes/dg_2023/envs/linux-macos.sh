#!/bin/sh

# This file setup an environment which allows you to compile the code
# on Linux or macOS using the default compiler (gcc or clang)
#
# HOW TO USE THIS FILE?
#   open a terminal
#   source ./envs/linux-macos.sh
#   mkdir build
#   cd build
#   cmake ..
#   make
#   [executables are built in the bin/ folder]

# Detect shell
if [ -n "$ZSH_VERSION" ]; then
	SOURCE=${(%):-%N} # from https://stackoverflow.com/questions/9901210/bash-source0-equivalent-in-zsh
elif [ -n "$BASH_VERSION" ]; then
	SOURCE=${BASH_SOURCE[0]}
else
	echo "Shell not recognized; only bash and zsh are supported!"
	exit 1
fi

# Get the absolute path of the root folder
DIR="$( cd "$( dirname $SOURCE )" && pwd && echo x)"; DIR="${DIR%x}"
abspath=$(dirname $DIR )

# set the location of gmsh SDK ( **MODIFY THIS LINE FOR YOUR SYSTEM** )
GMSHSDK=${abspath}/lib/gmsh-sdk
EIGEN=${abspath}/lib/eigen

# where are gmsh and gmsh-**.so ?
export PATH=${GMSHSDK}/bin:${GMSHSDK}/lib:${PATH}
# where is gmsh.h ?
export INCLUDE=${EIGEN}:${GMSHSDK}/include:${INCLUDE}
# where is gmsh.lib ?
export LIB=${GMSHSDK}/lib:${LIB}
# where is gmsh.py ? (required only if you want to use the python API)
export PYTHONPATH=${GMSHSDK}/lib:${PYTHONPATH}
# the following command is only useful for macOS 
export DYLD_LIBRARY_PATH=${GMSHSDK}/lib:${DYLD_LIBRARY_PATH}
