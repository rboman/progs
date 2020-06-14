#include <iostream>

using namespace std;

class A
{
    int data;

public:
    A(int i);
    int get() const;
    int &get();
};

A::A(int i) : data(i) {}

int
A::get() const
{
    cout << "A(" << data << ") int get() const" << endl;
    return data;
}

int &
A::get()
{
    cout << "A(" << data << ") int & get()" << endl;
    return data;
}

int
main()
{
    const A o1(1);
    A o2(2);

    o1.get();
    o2.get();

    return 0;
}