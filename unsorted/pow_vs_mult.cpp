// https://stackoverflow.com/questions/2940367/what-is-more-efficient-using-pow-to-square-or-just-multiply-it-with-itself

#include <cmath>
#include <chrono>
#include <iostream>
#include <random>

using Moment = std::chrono::high_resolution_clock::time_point;
using FloatSecs = std::chrono::duration<double>;

inline Moment now()
{
    return std::chrono::high_resolution_clock::now();
}

#define TEST(num, expression) \
double test##num(double b, long loops) \
{ \
    double x = 0.0; \
\
    auto startTime = now(); \
    for (long i=0; i<loops; ++i) \
    { \
        x += expression; \
        b += 1.0; \
    } \
    auto elapsed = now() - startTime; \
    auto seconds = std::chrono::duration_cast<FloatSecs>(elapsed); \
    std::cout << seconds.count() << "\t"; \
    return x; \
}

TEST(2, b*b)
TEST(3, b*b*b)
TEST(4, b*b*b*b)
TEST(5, b*b*b*b*b)

template <int exponent>
double testCppPow(double base, long loops)
{
    double x = 0.0;

    auto startTime = now();
    for (long i=0; i<loops; ++i)
    {
        x += std::pow(base, exponent);
        base += 1.0;
    }
    auto elapsed = now() - startTime;

    auto seconds = std::chrono::duration_cast<FloatSecs>(elapsed); \
    std::cout << seconds.count() << "\t"; \

    return x;
}

double testCPow(double base, double exponent, long loops)
{
    double x = 0.0;

    auto startTime = now();
    for (long i=0; i<loops; ++i)
    {
        x += ::pow(base, exponent);
        base += 1.0;
    }
    auto elapsed = now() - startTime;

    auto seconds = std::chrono::duration_cast<FloatSecs>(elapsed); \
    std::cout << seconds.count() << "\t"; \

    return x;
}

int main()
{
    using std::cout;
    long loops = 100000000l;
    double x = 0;
    std::random_device rd;
    std::default_random_engine re(rd());
    std::uniform_real_distribution<double> dist(1.1, 1.2);
    cout << "exp\tc++ pow\tc pow\tx*x*x...";

    cout << "\n2\t";
    double b = dist(re);
    x += testCppPow<2>(b, loops);
    x += testCPow(b, 2.0, loops);
    x += test2(b, loops);

    cout << "\n3\t";
    b = dist(re);
    x += testCppPow<3>(b, loops);
    x += testCPow(b, 3.0, loops);
    x += test3(b, loops);

    cout << "\n4\t";
    b = dist(re);
    x += testCppPow<4>(b, loops);
    x += testCPow(b, 4.0, loops);
    x += test4(b, loops);

    cout << "\n5\t";
    b = dist(re);
    x += testCppPow<5>(b, loops);
    x += testCPow(b, 5.0, loops);
    x += test5(b, loops);

    std::cout << "\n" << x << "\n";
}