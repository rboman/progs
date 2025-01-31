# Example of VSCode configuration files

These folders contain some examples of advanced configuration files of VSCode for linux, mac and windows.

They can be copied in the `.vscode/` folder of the project.

* `c_cpp_properties.json`: helps VSCode "Intellisense" find the header files such as `<gmsh.h>` or `<iostream>` if they appear underlined in red.
* `launch.json`: defines a "debug" configuration which allows for interactive debugging with the mouse in the GUI of VSCode.
* `settings.json`: defines the environment needed by CMake to build the projects, so that running `envs/windows.cmd` is not needed anymore.
