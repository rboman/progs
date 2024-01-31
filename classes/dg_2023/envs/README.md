# math0471/envs

This folder contains 2 scripts for the correct definition of environment variables of your system so that
* Your c++ compiler (g++) is available from the command line,
* Eigen and gmsh libraries and header files can be found by cmake,
* The gmsh executable can be used in the command line (by just typing `gmsh`).
* `cmake` and `make` can be used on Windows as if you were on linux/mac (see `cmake.cmd` and `make.cmd` for more information)
  
Use `windows.cmd` on Windows (run it or double-click on it)

Use `linux-macos.sh` on linux or macOS with the command:
```
source linux-macos.sh
```
Use `nic5.sh` on the [CECI clusters](https://www.ceci-hpc.be/clusters.html):
```
source nic5.sh
```
