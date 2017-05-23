//
// $Id$
//

#include "OMPTest.h"
#include <fstream>
#include <stdlib.h>

// test1
#include <omp.h>

// test2
#include "tbb/parallel_for.h"
#include "tbb/blocked_range.h"
#include "tbb/task_scheduler_init.h"


OMPTest1::OMPTest1() : OMPTest(), a(NULL), b(NULL), c(NULL)
{
    setMatrixSize();
    setLoops();
}

void
OMPTest1::setSize(int siz)
{
    setMatrixSize(siz); 
}

OMPTest1::~OMPTest1()
{
    freeMem();
}

double 
OMPTest1::getMem() const
{
    return ((sizem*sizen+sizem+sizen)*8.)/1024/1024; // in Mb
}

void
OMPTest1::allocMem()
{
    freeMem();

    try
    {
        a = new double[sizem];
        b = new double[sizem*sizen];
        c = new double[sizen];
    }
    catch(...)
    {
        std::cerr << "not enough memory (" << getMem() << "Mo needed)!\n";
        exit(1);
    }

    // init
    for(int i=0; i<sizem; ++i) a[i]=0.0;
    for(int i=0; i<sizem; ++i) 
       for(int j=0; j<sizem; ++j) 
            b[sizem*i+j]=i;
    for(int i=0; i<sizen; ++i) c[i]=2.0;
}

void
OMPTest1::freeMem()
{
    if(a) delete[] a;
    if(b) delete[] b;
    if(c) delete[] c;
}

void 
OMPTest1::setMatrixSize(int m, int n) 
{
    if(m<=0) return;
    sizem=m;
    sizen=n;
    if(n<=0) sizen=m;
}

void 
OMPTest1::setLoops(int in, int out) 
{ 
    loopin=in; 
    loopout=out;
}

void mxv(int m, int n, double *a, double *b, double *c, int nbt, int tmax)
{
    #pragma omp parallel for num_threads(nbt)
    for (int i=0; i<m; i++)
    {
        for(int t=0; t<tmax; ++t)
        {
        a[i] = 0.0;
        for (int j=0; j<n; j++)
            a[i] += b[i*n+j]*c[j];
        }
    } 
}

void 
OMPTest1::execute(int nbthreads)
{
    // 23s
    //vc:23, icc+win: 49 (icc+win+noinline: 8s!!)   
    //gcc:16.6 icpc: 57
    //mxv(sizem,sizen,a,b,c,nbthreads, loopin);

    
    //vc:49, icc+win: 49    
    //gcc:17 icpc: 54

    /*
    for(int tout=0; tout<loopout; ++tout)
    {
        #pragma omp parallel for num_threads(nbthreads)
        for (int i=0; i<sizem; i++)
        {
            for(int tin=0; tin<loopin; ++tin)
            {
                a[i] = 0.0;
                for (int j=0; j<sizen; j++)
                    a[i] += b[i*sizen+j]*c[j];
            }
        }
    }
    */

    /*
    int tmax=loopin;
    int tmax2=loopout;
    int m=sizem;
    int n=sizen;
    double *aa=a;
    double *bb=b;
    double *cc=c;
    int nbt=nbthreads;
    //vc:23, icc+win: 8    
    //gcc:16 icpc:9
    for(int tout=0; tout<tmax2; ++tout)
    {
        #pragma omp parallel for num_threads(nbt)
        for (int i=0; i<m; i++)
        {
            for(int tin=0; tin<tmax; ++tin)
            {
                aa[i] = 0.0;
                for (int j=0; j<n; j++)
                    aa[i] += bb[i*n+j]*cc[j];
            }
        }
    }
    */

    
    // version "optimale" tout compilateur

    double *aa=a; // ok pour icc+win si seulement cette nouvelle var.
    //double *bb=b;
    //double *cc=c;
    int m=sizem;  // ok avec vc en plus du aa
    int n=sizen;  // ok avec vc en plus du aa

    //vc:23, icc+win: 8    
    //gcc:16 icpc:9
    for(int tout=0; tout<loopout; ++tout)
    {
        #pragma omp parallel for num_threads(nbthreads)
        for (int i=0; i<m; i++)
        {
            for(int tin=0; tin<loopin; ++tin)
            {
                aa[i] = 0.0;
                for (int j=0; j<n; j++)
                    aa[i] += b[i*n+j]*c[j];
            }
        }
    }


    // --


}

double 
OMPTest1::flops(int nbthreads) const
{
    return double(sizem)*double(sizen)*double(loopin)*double(loopout);
}

// ------------------

OMPTest2::OMPTest2() : OMPTest1()
{
}

class MatMult
{
    int my_n;
    double *my_a;
    double *my_b;
    double *my_c; 
    int my_nbt; 
    int my_tmax;
public:
    MatMult(int n, double *a, 
        double *b, double *c, int nbt, int tmax) : my_n(n), my_a(a), my_b(b), my_c(c), my_nbt(nbt), my_tmax(tmax)
    {
    }
    void operator() (const tbb::blocked_range<int> &r) const 
    { 
        double *a=my_a;
        double *b=my_b;
        double *c=my_c;
        int tmax=my_tmax;
        int n=my_n;
        for(int i=r.begin(); i!=r.end(); ++i) 
        {
            for(int t=0; t<tmax; ++t)
            {
                a[i] = 0.0;
                for (int j=0; j<n; j++)
                    a[i] += b[i*n+j]*c[j];
            }
        }
    }   
};

void 
OMPTest2::execute(int nbthreads)
{
    tbb::task_scheduler_init init(nbthreads);

    tbb::parallel_for(tbb::blocked_range<int>(0, sizem ),
               MatMult( sizen,a,b,c, nbthreads, loopin ) );

}


// ------------------

OMPDataSet::OMPDataSet(int m, int n)
{
    data.resize(m);
    for(int i=0;i<m;++i)
        data[i].resize(n);
}

// ------------------

std::ostream &
operator<<(std::ostream &out, OMPData const &obj)
{
    out << obj.idx1 << '\t' 
        << obj.idx2 << '\t' 
        << obj.size << '\t' 
        << obj.nbt << '\t' 
        << obj.mem << '\t' 
        << obj.cpu << '\t' 
        << obj.flops << '\n';
    return out;
}

std::ostream &
operator<<(std::ostream &out, OMPDataSet const &obj)
{
    for(int i=0; i<obj.data.size(); ++i)
        for(int j=0; j<obj.data[i].size(); ++j) 
            out << obj.data[i][j];
    return out;
}


// ------------------

OMPTester::OMPTester(OMPTest &t) : test(t)
{
    setSizes();
    setThreads();
}

void 
OMPTester::execute() 
{
    // compute the total nb of tests
    int ntest1=0,ntest2=0;
    for(int siz=srange.getMin(); siz<=srange.getMax(); siz*=srange.getStep())
        ntest1++;
    for(int nbt=trange.getMin(); nbt<=trange.getMax(); nbt+=trange.getStep())
        ntest2++;

    // create the result table
    OMPDataSet results(ntest1,ntest2);

    // loop on sizes
    int idx1=0;
    for(int siz=srange.getMin(); siz<=srange.getMax(); siz*=srange.getStep())
    {    
        idx1++;
        test.setSize(siz);
        test.allocMem();

        // loop on thread nb
        int idx2=0;
        for(int nbt=trange.getMin(); nbt<=trange.getMax(); nbt+=trange.getStep())
        {
            idx2++;
            double tstart = omp_get_wtime();
            test.execute(nbt);
            double tstop = omp_get_wtime();
            double cpu = tstop-tstart;

            OMPData res = OMPData(idx1, idx2, siz, nbt, test.getMem(), cpu, test.flops(nbt));

            std::cout << res;

            results(idx1-1,idx2-1) = res;
        }
    }

    // print results
    //std::cout << results;

    // write results to file
    std::ofstream f("OMPTester.txt");
    f << results;
    f.close();
}


int main()
{
    OMPTest1 test;   // OpenMP
    //OMPTest2 test;  // TBB
    test.setLoops(100,1);

    OMPTester tester(test);
    //tester.setSizes(OMPRange(12800, 12801, 2));
    tester.setSizes(OMPRange(10000, 10001, 2));
    tester.setThreads(OMPRange(1, 8));
    tester.execute();

    return 0;
}

