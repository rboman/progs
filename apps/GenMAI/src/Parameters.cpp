#include "Global.h"
#include "Parameters.h"

/**
 * @brief Constructor
 */

Parameters::Parameters() 
{

}

Parameters::Parameters(const Parameters &obj)
{
    MyMap::const_iterator it;
    for(it=obj.paramMap.begin(); it!=obj.paramMap.end(); ++it)
        paramMap[it->first] = it->second->clone();
}

void
Parameters::operator=(const Parameters &obj)
{
    MyMap::const_iterator it;
    for(it=obj.paramMap.begin(); it!=obj.paramMap.end(); ++it)
        paramMap[it->first] = it->second->clone();
}

Parameters::~Parameters() 
{
    MyMap::const_iterator it;
    for(it=paramMap.begin(); it!=paramMap.end(); ++it)
        delete it->second;
}

/**
 * @brief Adds a Layer (used by the constructor)
 */

void 
Parameters::addParam(const Param &param)
{
    paramMap[param.getId()] = param.clone();
}

/**
 * @brief Main function : gets a Layer objects from the singleton
 */

Param const &
Parameters::get(const ParamEnum t) const
{
    MyMap::const_iterator it;
    it = paramMap.find(t);
    if(it!=paramMap.end())
        return *(it->second);
    else
        return Param::getNull();
}

Param &
Parameters::get(const ParamEnum t)
{
    return *(paramMap[t]);
}

/**
 * @brief Prints the parameters to stdout
 */

void 
Parameters::print() const
{
    std::cout << "PARAMETRES:" << std::endl;
    std::cout << "-----------" << std::endl;

    MyMap::const_iterator it;
    for(it=paramMap.begin(); it!=paramMap.end(); ++it)
    {
        it->second->print();
        std::cout << std::endl;
    }
}

void 
Parameters::save(const std::string &file) const
{
    FILE *fich = fopen(file.c_str(),"w+t");

    MyMap::const_iterator it;
    for(it=paramMap.begin(); it!=paramMap.end(); ++it)
        it->second->save(fich);

    fclose(fich);
}

void 
Parameters::load(const std::string &file)
{
    std::cout << "opening file " << file << "\n";
    FILE *fich = fopen(file.c_str(),"r+t");

    MyMap::const_iterator it;
    for(it=paramMap.begin(); it!=paramMap.end(); ++it)
        it->second->load(fich);

    fclose(fich);
}
