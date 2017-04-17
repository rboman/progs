#ifndef PARAM_H
#define PARAM_H

#include <string>
#include "Point.h"
#include "LayerType.h"
#include "ParamEnum.h"

/**
 * @brief Generic Parameter with print, load and save functions
 */

class Param
{
    ParamEnum   id;
    std::string name;
public:
    Param(ParamEnum id=(ParamEnum)0, const std::string &name="");
    virtual void setToDefault();

    std::string getName() const;
    ParamEnum   getId() const;

    static Param &getNull();

    virtual Param * clone() const;
    virtual void print() const;
    virtual void load(FILE *file);
    virtual void save(FILE *file) const;

    // Get/Set

    virtual int getInt() const;
    virtual void set(int value);

    virtual double getDouble() const;
    virtual void set(double value);

    virtual Point const &getPoint() const;
    virtual Point &getPoint();
    virtual void set(Point value);

    virtual LayerType getLayer(int i) const;
    virtual void      add(LayerType value);
    virtual size_t size() const;

protected:
    void   saveDouble(FILE *file, double  val, bool newline) const;
    double loadDouble(FILE *file, bool newline) const;

    void saveInteger(FILE *file, int  val, bool newline) const;
    int  loadInteger(FILE *file, bool newline) const;

    void saveNewLine(FILE *file) const;
    void loadNewLine(FILE *file) const;
    void loadEmptyLine(FILE *file) const;
};

#endif

