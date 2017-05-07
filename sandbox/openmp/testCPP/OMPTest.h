//
// $Id$
//

#ifndef OMPTEST_H
#define OMPTEST_H

#include <iostream>
#include <vector>

class OMPTest
{
public:
    virtual ~OMPTest() {}
    virtual void execute(int nbthreads) = 0;
    virtual double flops(int nbthreads) const= 0;
    virtual void setSize(int siz)=0;
    virtual void allocMem()=0;
    virtual void freeMem()=0;
    virtual double getMem() const=0;
};


class OMPTest1 : public OMPTest
{
protected:
    double *a,*b,*c;
    int sizem;
    int sizen;
    int loopin;
    int loopout;
public:
    OMPTest1();
    virtual ~OMPTest1();

    void setMatrixSize(int m=100, int n=-1);
    void setLoops(int in=1, int out=1);

    virtual void execute(int nbthreads);
    virtual double flops(int nbthreads) const;
    virtual void setSize(int siz);
    virtual void allocMem();
    virtual void freeMem();
    virtual double getMem() const;
};

class OMPTest2 : public OMPTest1
{
public:
    OMPTest2();
    virtual void execute(int nbthreads);
};

class OMPRange
{
    int rmin;
    int rmax;
    int rstep;
public:
    OMPRange(int vmin=0, int vmax=0, int vstep=1) { rmin=vmin; rmax=vmax; rstep=vstep; }
    int getMin() const { return rmin;}
    int getMax() const { return rmax;}
    int getStep() const { return rstep;}
};

// --------------------------------------------------
class OMPData
{
    int idx1;
    int idx2;
    int size;
    int nbt;
    double mem;
    double cpu;
    double flops;
public:
    OMPData(int _idx1=0, int _idx2=0, int _size=0, int _nbt=0, double _mem=0, double _cpu=0, double _flops=0) :
      idx1(_idx1), idx2(_idx2), size(_size), nbt(_nbt), mem(_mem), cpu(_cpu), flops(_flops) {} 
    friend std::ostream &operator<<(std::ostream &out, OMPData const &obj);
};

class OMPDataSet
{
    std::vector< std::vector<OMPData> > data;
public:
    OMPDataSet(int m, int n);
    OMPData &operator()(int i, int j) { return data[i][j]; }
    friend std::ostream &operator<<(std::ostream &out, OMPDataSet const &obj);
};

// --------------------------------------------------

class OMPTester
{
    OMPTest &test;
    OMPRange trange;  // thread range
    OMPRange srange;  // size range
public:
    OMPTester(OMPTest &t);

    void setSizes(OMPRange r=OMPRange(100, 20000, 2)) { srange=r;}
    void setThreads(OMPRange r=OMPRange(1, 8, 1)) { trange=r;}
    void execute();
private:

};


#endif //OMPTEST_H


