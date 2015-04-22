//
// from http://www.zedwood.com/article/cpp-md5-function
//
//compile and run in linux:
// g++ main.cpp md5.cpp -o md5_sample && ./md5_sample
//output:
// md5 of 'grape': b781cbb29054db12f88f08c6e161c199
//

#include <iostream>
#include "md5.h"
 
using std::cout; using std::endl;
 
int main(int argc, char *argv[])
{
    cout << "md5 of 'grape': " << md5("grape") << endl;
    return 0;
}
