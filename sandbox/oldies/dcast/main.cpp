#include <iostream>
#include <typeinfo>

using namespace std;

class Arg
{
    int i;

  public:
    Arg(int _i = 1) : i(_i) {}
};

class A
{
    int data;

  public:
    A() : data(0)
    {
        cout << "building A" << endl;
    }
    virtual ~A() {}
    void printA()
    {
        cout << "I am A" << endl;
    }
};

template <class T>
class B : public virtual A
{
    T data2;

  public:
    B() : A()
    {
        cout << "building B" << endl;
    }
    void printB()
    {
        cout << "I am B" << endl;
    }
    virtual void printD() = 0;
};

template <class T>
class C : public B<T>
{
    T data3;

  public:
    C() : B<T>()
    {
        cout << "building C" << endl;
    }
    void printC()
    {
        cout << "I am C" << endl;
    }
};
template <class T>
class D : public C<T>
{
    T data4;

  public:
    D() : C<T>()
    {
        cout << "building D" << endl;
    }
    void printD()
    {
        cout << "I am D" << endl;
    }
};

int main()
{
    D<Arg> *obj = new D<Arg>();

    cout << "obj=" << obj << endl;

    A *objA = dynamic_cast<A *>(obj);
    cout << "A = " << objA << endl;
    B<Arg> *objB = dynamic_cast<B<Arg> *>(obj);
    cout << "B = " << objB << endl;
    C<Arg> *objC = dynamic_cast<C<Arg> *>(obj);
    cout << "C = " << objC << endl;
    D<Arg> *objD = dynamic_cast<D<Arg> *>(obj);
    cout << "D = " << objD << endl;

    if (objB)
    {
        objB->printB();
    }
    else
    {
        cout << "rtti failure!" << endl;
    }

    cout << "typeid A=" << typeid(objA).name() << endl
         << flush;
    cout << "typeid B=" << typeid(objB).name() << endl
         << flush;
    cout << "typeid C=" << typeid(objC).name() << endl
         << flush;
    cout << "typeid D=" << typeid(objD).name() << endl
         << flush;
}
