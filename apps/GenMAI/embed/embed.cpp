//
// Python embedded into GenMai
//

#include <iostream>
#include <Python.h>

extern "C" {
    void init_utils(void); 
}

void printBanner();
void bye()
{
    std::cout << "Bye!" << std::endl;
}

int
main(int argc, char *argv[])
{
    printBanner();
    std::cout << " init python..." << std::endl;
    Py_Initialize();
    Py_SetProgramName(argv[0]);
    PySys_SetArgv(argc,argv);

    Py_AtExit(bye);
    std::cout << " init & import GenMai module..." << std::endl;
    init_utils();

    PyRun_SimpleString("import utils \n");
    //PyRun_SimpleString("print dir(); \n");
    
    std::cout << " starting python..." << std::endl << std::endl;
    //std::cout << Py_GetVersion() << std::endl;
    PyRun_InteractiveLoop(stdin, "<stdin>");

    Py_Finalize();
    return 0;
}

