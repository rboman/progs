//
// $Id$
//

#ifndef T_IOOBJECT_H
#define T_IOOBJECT_H

#include <map>
#include <string>
#include "Param.h"

/**
 * @brief Mother class for Parameters (I/O routines)
 */

class Parameters
{
private:
    typedef std::map<ParamEnum, Param*> MyMap ;   // ptr car on utilise fcts virtuelles
    MyMap paramMap;
    
protected:
    void addParam(const Param &param);

public:
    Parameters();

    Parameters(const Parameters &obj);
    void operator=(const Parameters &obj);
    virtual ~Parameters();

    void load(const std::string &file);
    void save(const std::string &file) const;
    void print() const;

    Param const &get(const ParamEnum t) const;
    Param &get(const ParamEnum t);
};

#endif

