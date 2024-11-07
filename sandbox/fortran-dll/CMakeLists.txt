# Déclaration de la version minimum de CMake
cmake_minimum_required(VERSION 3.10)

# Nom du projet
project(FortranDLLExample LANGUAGES Fortran)

# Options pour le compilateur
#set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC")

# Cible pour la DLL
add_library(addition SHARED addition.f90 module1.f90 module2.f90)
#set_target_properties(addition PROPERTIES OUTPUT_NAME "addition")

# Cible pour le programme principal
add_executable(main main.f90)

# Lien du programme principal avec la DLL
target_link_libraries(main PRIVATE addition)
