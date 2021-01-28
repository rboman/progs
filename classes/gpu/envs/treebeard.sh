
# try to get the absolute path of root folder
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd && echo x)"; DIR="${DIR%x}"
abspath=$(dirname $DIR )

source ${abspath}/envs/linux-macos.sh
source ${abspath}/envs/pgi.sh

