#ifndef UNIC_H
#define UNIC_H

#include <string>
#include <vector>
#include <iostream>

void fct(std::string const &str);


class ObjA
{
public:
    ObjA() {}
    ObjA(ObjA const&) = delete;
    void operator=(ObjA const&) = delete;

    size_t __hash__()
    {
        size_t val=reinterpret_cast<size_t>(this);
        return val;
    }
    bool __cmp__(ObjA const *obj) // python 2
    {
        size_t v1=reinterpret_cast<size_t>(this);
        size_t v2=reinterpret_cast<size_t>(obj);
        return v1<v2;
    }
    bool __eq__(ObjA const *obj) // python 3
    {
        size_t v1=reinterpret_cast<size_t>(this);
        size_t v2=reinterpret_cast<size_t>(obj);
        return v1==v2;
    }

};

class ListA
{
    std::vector<ObjA*> objs;
public:
    ListA() {}
    void push(ObjA *o) { 
        objs.push_back(o); 
        std::cout << "[ListA] object " << o << " added\n";
        }
    ObjA *get(size_t i) const { return objs[i]; }
};



#endif //UNIC_H