//
// $Id$
//

%module castswig
%{
#include <iostream>
%}

%include "std_vector.i"

%inline {
class A
{
public:
	A() { std::cout << "A constructor\n"; }
};
}

%template() std::vector<A*>;

%inline {
class B
{
public:
	B(const std::vector<A*> &listA) { std::cout << "B constructor\n"; }
};
}


