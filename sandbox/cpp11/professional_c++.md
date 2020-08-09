# Notes lecture Professional C++


## namespaces
c++17: definition simplifiée des templates imbriqués.
namespace A::B::C {

}

namespace abc = A::B::C;   // alias

using std::cout; // permet d'utiliser "cout" 

## types
c++17: std::byte (mieux que unsigned char)

## strongly typed enum:
enum class PieceType
{
    King = 1,
    Queen,
    Rook = 10,
    Pawn
};

## initializers for if/switch statements

if( int a = get_a(); a<10) { }

## functions

c++14: function return type deduction (auto):
auto f(int a)
{
    return a+2;
}

## stack-based arrays
int myArray[3];         // uninitialised!
int myArray[3] = {};    // initialise to 0
int myArray[] = {1, 2, 3};

std::size(myArray) // stack-based array size (c++17)

HINT: myArray[3] => use std::array<int,3> (iterators)!

# Structured Bindings (c++17)

std::array<int, 3> values = { 11, 22, 33 };
auto [x, y, z] = values;  // declares 3 variables x,y,z

struct Point { double mX, mY, mZ; };
auto [x, y, z] = point;

works with arrays, struct, pair, tuples.

# range-based for loop

std::array<int, 4> arr = {1, 2, 3, 4};
for (int i : arr) {
    std::cout << i << std::endl;
}

# initializer lists

#include <initializer_list>

int makeSum(initializer_list<int> lst)
{ ... }
int a = makeSum({1,2,3});

# Null Pointer Constant

nullptr // replacement of NULL (c++11)

# smart pointers

#include <memory>

replace raw pointers by:
    std::unique_ptr
    std::shared_ptr

c++14:
auto a = make_unique<A>();      // A *a = new A;
[c++11: unique_ptr<A> anEmployee(new A);]

arrays:
auto a = make_unique<A[]>(10); // a[0], a[1], ...

auto a = make_shared<A>();    // A *a = new A;

## type inference

auto, decltype

auto strips away reference and const qualifiers (thus it creates a copy)
    => "const auto &"

decltype:
int x = 123;
decltype(x) y = 456;

## classes

member variables can in initialised in the class declaration (.h): 

private:
    std::string a = "blabla";

### uniform initialization

before c++11:
CircleStruct myCircle1 = {10, 10, 2.5};
CircleClass myCircle2(10, 10, 2.5);
c++11:
CircleClass myCircle4 = {10, 10, 2.5}; // calls constructor directly as a struct
CircleClass myCircle4{10, 10, 2.5}; // same behaiour

int a{3};   // initialise to 3
int a{};    // initialise to 0
int x = {3.14}; // error (narrowing)!
f({3.14})       // error if argument type is int
int* pArray = new int[4]{0, 1, 2, 3}; // works with almost anything!

## misc

the book uses #pragma once in .h files
member variables => start with m

# strings

## raw string literals

char* ptr = "hello"; // a pointer to a string literal "hello" => cannot be modified (should be "const char*")
char arr[] = "hello"; // a character array. The string literal is copied; can be modified

raw string literals: starts with R("   ends with )"
    const char* str = R"(may contain " - escape char are not translated)";
the parenthesis can be replaced by any string+( and string+):
    const char* str = R"-( may contain )" !! )-";

## std::string Literals

using namespace std::string_literals;
auto string2 = "Hello World"s;

## functions

std::to_string(5)  => "5"

int std::stoi("2");
float std::stof("3.14");
double std::stod("5.0e-8");

c++17: <charconv>: low level optimised fcts => std::to_chars & std::from_chars

## std::string_view (c++17)

but: uniformiser "const char*" et "const std::string&" en tant qu'argument de fct.
     => utiliser plutôt ça que "std::string const &"

<string_view> header
std::string_view: 
- equivalent to "pointer to a string (whatever its type) + length".
- do not copy the string or create a new string => more efficient
- usually passed by value
- same methods as std::string.

using namespace std::string_view_literals;
auto sv = "test"sv;

# coding style

ideas:
- names beginning with 1 underscore are reserved and shall not be used!
- global names starts with a 'g'
- members name starts with a 'm'
- bools starts with 'is' or 'b'
- constants starts with 'k'
- static starts with 's'

