#include <iostream>

extern "C"
{
    void froutine(int *entier, char const *chainec, int const *chainec_length);
}

int main()
{
    std::string chaine = "Romain";

    std::cout << "in main()\n";
    int entier = 2;
    char const *chainec = chaine.c_str();
    int chainec_length = chaine.size();
    froutine(&entier, chainec, &chainec_length);
    std::cout << "in main()\n";
    return 0;
}