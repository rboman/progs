
#include <iostream>
#include <vector>

using namespace std;

struct MyObject
{
    // data
    int data;

    // constructors
    MyObject(int _data)
    {
        cout << "constructor called!" << endl;
        data = _data;
    }
    MyObject(const MyObject &object)
    {
        cout << "copy constructor called!" << endl;
        data = object.data;
    }
    MyObject &operator=(const MyObject &object)
    {
        cout << "operator= called!" << endl;
        data = object.data;
        return *this;
    }

    // i/o
    friend ostream & operator<< (ostream &mystream, const MyObject &obj)
    {
        mystream << obj.data << endl ;
        return mystream;
    }

};


int main()
{
    std::vector<MyObject *> set;

    MyObject obj1(1),obj2(2);

    set.push_back(&obj1);
    set.push_back(&obj2);

    // affichage de set

    cout << "obj1 = " << *(set[0]) ;
    cout << "obj2 = " << *(set[1]) ;

    // copie du set via operator=

    std::vector<MyObject *> set2;
    
    set2 = set;

    // modification des objets via set2

    set2[0]->data = 11;
    set2[1]->data = 12;

    // affichage de set

    cout << "obj1 = " << *(set[0]) ;
    cout << "obj2 = " << *(set[1]) ;

    return 0;
}

