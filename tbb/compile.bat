
rem toutes ces commandes marchent! le .def ne sert a rien
rem a mon avis ca pourrait servir uniqut si on voulait linker avec MSVC...

rem g++ count_strings.cpp -IC:\local\tbb43_20150424oss\include -LC:\local\tbb43_20150424oss\build\windows_ia32_gcc_mingw4.8.1_release -ltbb

g++ count_strings.cpp -IC:\local\tbb43_20150424oss\include C:\local\tbb43_20150424oss\build\windows_ia32_gcc_mingw4.8.1_release\tbb.dll

rem g++ -g count_strings.cpp -IC:\local\tbb43_20150424oss\include C:\local\tbb43_20150424oss\build\windows_ia32_gcc_mingw4.8.1_debug\tbb_debug.dll

