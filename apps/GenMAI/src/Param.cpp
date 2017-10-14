//   Copyright 2003-2017 Romain Boman
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

#include "Global.h"
#include "Param.h"

Param &
Param::getNull()
{
    static Param nul((ParamEnum)0,"null");
    return nul;
}

Param::Param(ParamEnum id, const std::string &name) : id(id), name(name)
{
}

int 
Param::getInt() const
{
    exit(1);   
}

double 
Param::getDouble() const
{
    exit(1);
}

Point const &
Param::getPoint() const
{
    exit(1);
}

Point &
Param::getPoint()
{
    exit(1);
}

std::string 
Param::getName() const
{
    return name;
}

ParamEnum   
Param::getId() const
{
    return id;
}


void 
Param::set(int value)
{
    exit(1);
}

void 
Param::set(double value)
{
    exit(1);
}

void 
Param::set(Point value)
{
    exit(1);
}

Param * 
Param::clone() const
{
    return new Param(*this);
}

LayerType
Param::getLayer(int i) const
{
    exit(1);
}

void 
Param::add(LayerType value)
{
    exit(1);
}

size_t
Param::size() const
{
    return 1;
}

void 
Param::print() const
{
    std::cout << getName().c_str() << " : ";
}

/**
 * @brief Saves a double to the given file
 */

void 
Param::saveDouble(FILE *file, double val, bool newline) const
{
    fprintf(file, "%16.8E", val);
    if(newline) saveNewLine(file);
}

/**
 * @brief Loads a double to the given file
 */

double 
Param::loadDouble(FILE *file, bool newline) const
{
    double val;
    fscanf(file, "%16lE", &val);
    if(newline) loadNewLine(file);
    return val;
}

/**
 * @brief Saves a int to the given file
 */

void 
Param::saveInteger(FILE *file, int val, bool newline) const
{
    fprintf(file, "%8d", val);
    if(newline) saveNewLine(file);
}

/**
 * @brief Loads a int to the given file
 */

int 
Param::loadInteger(FILE *file, bool newline) const
{
    int val;
    fscanf(file, "%8d", &val);
    if(newline) loadNewLine(file);
    return val;
}

/**
 * @brief Saves a newline
 */

void 
Param::saveNewLine(FILE *file) const
{
    fprintf(file, "\n");
}

/**
 * @brief Loads a newline
 */

void 
Param::loadNewLine(FILE *file) const
{    
    fscanf(file, "\n");
}

/**
 * @brief Go to the end of the line
 */

void 
Param::loadEmptyLine(FILE *file) const
{    
    char buf;
    while((buf = fgetc(file))!='\n') ;
}

/**
 * @brief Loads the parameters from a given file
 */

void 
Param::load(FILE *file)
{
    loadEmptyLine(file);    
}

/**
 * @brief Saves the parameters to a given file
 */

void 
Param::save(FILE *file) const
{ 
    fprintf(file, "%s\n", getName().c_str());
}

void 
Param::setToDefault() 
{

}

