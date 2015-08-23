//
// Teste les fct statiques dans les templates
//
// RoBo - juin 2002
//

#ifndef STAT_H
#define STAT_H

#include <iostream>

// Structure bidon qu'on va declarer statique dans la classe Stat
struct MyS
{
    int i;
    MyS(int _i=0): i(_i) {}
};

// superbe template "Stat" (parametre = classe) 

template<class T>
class Stat
{
 public:
    static int a; // variable statique de type predefini (bete int)
    static MyS s; // variable statique de type "classe utilisateur"
    T var;        // variable pour utiliser le param du template
    
    Stat(T _var) : var(_var) {}
    
    void print()
    {
        std::cout << "var=" << var << std::endl;
        std::cout << "a=" << a << std::endl;
        std::cout << "s.i=" << s.i << std::endl;
    }
    
};

// machin qu'il faut rajouter sous SGI pour que ca linke bien
// sinon, on n'arrive pas a appeler le constructeur sans arguments
// (ca marche sur toutes les machines)
template<class T> MyS Stat<T>::s;

// superbe template "StatI" (parametre = classe, int) 

template<class T, int n>
class StatI
{
 public:
    static int a; // variable statique de type predefini (bete int)
    static MyS s; // variable statique de type "classe utilisateur"
    T var;        // variable pour utiliser le param du template
   
    StatI(T _var) : var(_var) {}
    
    void print()
    {
        std::cout << "var,n=" << var << "," << n << std::endl;
        std::cout << "a=" << a << std::endl;
        std::cout << "s.i=" << s.i << std::endl;
    }
    
};

// machin qu'il faut rajouter sous SGI pour que ca linke bien
// sinon, on n'arrive pas a appeler le constructeur sans arguments
// (c'est ignore sur toutes les autres machines)
template<class T, int n> MyS StatI<T,n>::s;


#endif
