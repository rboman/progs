// This program takes a .geo file as argument, and reads the onelab parameters
// defined by the "SetNumber" command.
// This technique can be used to define additional parameters for the solver
// such as the values for the boundary conditions or material parameters.
//
// How to build/run? (windows)
//   [in examples/gmsh_api]
//   mkdir build
//   cd build
//   cmake .. && make && code7_parameters.exe ..\rectangle.geo

#include <gmsh.h>
#include <iostream>
#include <sstream> // for std::stringstream

int main(int argc, char **argv)
{
    // the program requires 1 argument: a .geo file.
    if (argc < 2)
    {
        std::cout << "Usage: " << argv[0] << " <geo_file>\n";
        return 0;
    }

    gmsh::initialize();

    gmsh::open(argv[1]);

    // get selected keys from the onelab database
    std::vector<std::string> keys;
    gmsh::onelab::getNames(keys, "(Boundary Conditions|Materials).+");
    for (auto &key : keys)
    {
        // get corresponding value
        std::vector<double> value;
        gmsh::onelab::getNumber(key, value);

        // print received data
        std::cout << key << ":";
        for (auto &v : value)
            std::cout << " " << v;
        std::cout << '\n';

        // expected key structure is "type/group_name/field"
        // => split the key string into components
        std::stringstream ss(key);
        std::vector<std::string> words;
        std::string word;
        while (std::getline(ss, word, '/')) // read string until '/'
            words.push_back(word);
        if (words.size() == 3)
        {
            std::cout << "\t.type of data = '" << words[0] << "'\n";
            std::cout << "\t.physical group name = '" << words[1] << "'\n";
            std::cout << "\t.field = '" << words[2] << "'\n";
            std::cout << "\t.value = " << value[0] << '\n';
        }
    }

    gmsh::finalize();
    return 0;
}
