//
// from http://www.zedwood.com/article/cpp-md5-function
//
//compile and run in linux:
// g++ main.cpp md5.cpp -o md5_sample && ./md5_sample
//output:
// md5 of 'grape'   : b781cbb29054db12f88f08c6e161c199
// md5 of 'gr'+'ape': b781cbb29054db12f88f08c6e161c199

#include <iostream>
#include "md5.h"
 
int main(int argc, char *argv[])
{
    std::cout << "md5 of 'grape'   : " << md5("grape") << '\n';

    // test2

    MD5 m = MD5();
    m.update("gr");
    m.update("ape");
    m.finalize();

    std::cout << "md5 of 'gr'+'ape': " << m.hexdigest() << '\n';

    return 0;
}
