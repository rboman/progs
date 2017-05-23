#include <iostream>
#include <vector>
#include <list>
#include <cstdlib>

void *operator new(size_t siz) throw(std::bad_alloc)
{
    std::cout << "new: " << siz << " bytes" << std::endl;
    return malloc(siz);
}

void *operator new[](size_t siz) throw(std::bad_alloc)
{
    std::cout << "new[]: " << siz << " bytes" << std::endl;
    return malloc(siz);
}

void operator delete(void *p)
{
    free(p);
}

class Obj
{
  public:
    int i, j, k, l; // 4*4 = 16bytes
};

int main()
{
    typedef std::list<Obj> MyList;
    //typedef int MyList;
    typedef std::vector<MyList> MyVector;

    MyVector *pipo;

    pipo = new MyVector;
    std::cout << "resize\n";
    pipo->resize(10);
    std::cout << "sizeof(Obj)     =" << sizeof(Obj) << std::endl;
    std::cout << "sizeof(MyVector)=" << sizeof(MyVector) << std::endl;
    std::cout << "sizeof(MyList)  =" << sizeof(MyList) << std::endl;
    std::cout << "capacity=" << pipo->capacity() << std::endl;
    std::cout << "size    =" << pipo->size() << std::endl;

    pipo->push_back(MyList());
    std::cout << "capacity=" << pipo->capacity() << std::endl;
    std::cout << "size    =" << pipo->size() << std::endl;

    //--
    std::cout << "**test 2" << std::endl;
    std::vector<int> test(1);
    //test.push_back(0);
    std::cout << &test << std::endl;

    std::cout << "**test 3" << std::endl;
    std::list<int> test2(0, 0);
    std::cout << "push" << std::endl;
    test2.push_back(1);
    test2.push_back(2);
    test2.push_back(4);
    test2.push_back(5);

    std::cout << "**test4" << std::endl;
    std::list<int> *test3 = new std::list<int>;
    std::cout << "push" << std::endl;
    test3->push_back(5);

    return 0;
}
