#include <iostream>

int
memtest()
{
    // memtest.cpp ---
    long int step = 1024 * 1024;
    double *ptr = NULL;

    long int lastsiz = 0;

    int k = 0;
    do
    {
        long int size = (++k) * step;
        std::cout << "trying to allocate "
                  << (size * sizeof(double)) / 1024 / 1024;
        std::cout << "Mo..." << std::flush;
        try
        {
            ptr = new double[size];
            if (ptr)
            {
                std::cout << "succes." << std::endl << std::flush;
                lastsiz = size;
                delete[] ptr;
            }
            else
            {
                std::cout << "failure." << std::endl << std::flush;
            }
        }
        catch (...)
        {
            std::cout << "failure." << std::endl << std::flush;
            break;
        }
    } while (ptr);

    std::cout << "** Metafor::memtest: maximum (estimated) memory block  : ";
    std::cout << (lastsiz * sizeof(double)) / 1024 / 1024 << "Mo..."
              << std::endl;

    // cout << "hit something followed by <ENTER>" << endl;
    // cin >> k;
    // memtest.cpp ---

    return lastsiz * (sizeof(double)) / (1024 * 1024);
}

int
main()
{
    return memtest();
}
