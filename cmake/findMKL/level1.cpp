
#include <mkl.h>
#include <iostream>
#include <vector>
#include <cassert>

std::ostream &operator <<(std::ostream &out, std::vector<double> const &v)
{
    out << '[';
    for(auto it=v.begin(); it!=v.end(); ++it)
    {
        out << *it;
        if(it+1!=v.end()) out << ", ";
    }
    out << ']';
    return out;
}

std::vector<double>
mydaxpy(double a, std::vector<double> const &x, std::vector<double> const &y)
{
    assert(x.size()==y.size());
    auto n = x.size();
    std::vector<double> res(n);
    for(size_t i=0; i<n; ++i)
        res[i] = a*x[i]+y[i];
    return res;
}

int main(int argc, char **argv)
{
    std::vector<double> x { 2.0, 1.0, -1.0 };
    std::vector<double> y { 1.0, 2.0, 3.0 };
    MKL_INT n = (MKL_INT)x.size();

    double a = 2.0;
    MKL_INT incx = 1;
    MKL_INT incy = 1;

    std::cout << "a=" << a << '\n';
    std::cout << "x=" << x << '\n';
    std::cout << "y=" << y << '\n';

    auto y2 = mydaxpy(a, x, y);
    std::cout << "mydaxpy=" << y2 << '\n';

    cblas_daxpy (n, a, &(x[0]), incx, &(y[0]), incy);
    std::cout << "daxpy=" << y << '\n';

    return 0;
}

