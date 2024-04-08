#ifndef CPPBUF2PY_H
#define CPPBUF2PY_H

#include "sph.h"
#include <iostream>

// http://bo-peng.blogspot.be/2004/10/how-to-re-direct-cout-to-python_05.html
// https://docs.python.org/2/c-api/sys.html#system-functions

/**
 * @brief redirect a std::stream to python
 */
#ifndef SWIG
class CppBuf2Py : public std::basic_streambuf<char>
{
public:
    typedef std::char_traits<char> traits_type;
    typedef traits_type::int_type int_type;

private:
    std::ostream &stream;
    std::streambuf *oldbuf;
    bool verb;
    bool errstream;

public:
    CppBuf2Py(std::ostream &o1, bool err = false);
    ~CppBuf2Py();

    static void test();

protected:
    virtual std::streamsize xsputn(const char *s, std::streamsize n);
    virtual int_type overflow(int c);
};
#endif //SWIG

/**
 * @brief redirect std::cout/std::cerr to python sys.stdout/sys.stderr
 */

class StdOutErr2Py
{
    CppBuf2Py out;
    CppBuf2Py err;

public:
    StdOutErr2Py();

    static void test();
};

#endif //CPPBUF2PY_H
