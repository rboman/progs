# This file setup an environment which allows you to compile the code
# on Linux or macOS using the default compiler (gcc or clang)


# try to get the absolute path of root folder
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd && echo x)"; DIR="${DIR%x}"
abspath=$(dirname $DIR )
#echo "abspath=${abspath}"

# set the location of the pixel game engine
OLCPATH=${abspath}/externals/olcPixelGameEngine

export INCLUDE=${OLCPATH}:${INCLUDE}
