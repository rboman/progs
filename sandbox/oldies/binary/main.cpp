// Binary.cpp : Defines the entry point for the console application.
//

#include <iostream>

bool
isLittleEndian()
{
    int x = 1;
    return (*(char *)&x == 1);
}

void
LittleEndianToNative(char *data, unsigned int size)
{
    if (isLittleEndian())
        return;
    char *sptr = data;
    char *eptr = data + size - 1;
    while (sptr <= eptr - 1)
    {
        char tmp = *sptr;
        *(sptr++) = *eptr;
        *(eptr--) = tmp;
    }
}

void
NativeToLittleEndian(char *data, unsigned int size)
{
    LittleEndianToNative(data, size);
}

void
BigEndianToNative(char *data, unsigned int size)
{
    if (!isLittleEndian())
        return;
    char *sptr = data;
    char *eptr = data + size - 1;
    while (sptr <= eptr - 1)
    {
        char tmp = *sptr;
        *(sptr++) = *eptr;
        *(eptr--) = tmp;
    }
}

void
NativeToBigEndian(char *data, unsigned int size)
{
    BigEndianToNative(data, size);
}

// --------------------------------------------------------------------

bool
isCharUnSigned()
{
    char c = 255;
    return (c > 128);
}

// --------------------------------------------------------------------

int
main(int argc, char *argv[])
{
    std::cout << "machine is " << (isLittleEndian() ? "little" : "big")
              << "endian\n";
    std::cout << "char is " << (isCharUnSigned() ? "un" : "") << "signed\n";

    std::cout << "sizeof(char)               = " << sizeof(char) << '\n';
    std::cout << "sizeof(short)              = " << sizeof(short) << '\n';
    std::cout << "sizeof(int)                = " << sizeof(int) << '\n';
    std::cout << "sizeof(long)               = " << sizeof(long) << '\n';
    std::cout << "sizeof(float)              = " << sizeof(float) << '\n';
    std::cout << "sizeof(double)             = " << sizeof(double) << '\n';
    std::cout << "sizeof(size_t)             = " << sizeof(size_t) << '\n';
    std::cout << "sizeof(void*)              = " << sizeof(void *) << '\n';
    std::cout << "sizeof(bool)               = " << sizeof(bool) << '\n';
#ifdef WIN32
    std::cout << "sizeof(unsigned __int64)   = " << sizeof(__int64) << '\n';
#else
    std::cout << "sizeof(unsigned long long) = " << sizeof(unsigned long long)
              << '\n';
#endif

    return 0;
}
