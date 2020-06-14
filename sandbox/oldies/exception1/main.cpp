//
// Test des exceptions
//

#include <iostream>
#include <limits>

using namespace std;

struct Range_error
{
    int i;
    Range_error(int ii) { i = ii; }
};

char
to_char(int i)
{
    if (i < std::numeric_limits<char>::min() ||
        std::numeric_limits<char>::max() < i)
        throw Range_error(i);
    return i;
}

int
main()
{
    try
    {
        char c = to_char(1000);
        cout << "c = " << c << endl;
    }
    catch (Range_error x)
    {
        cerr << "erreur i=" << x.i << "\n";
    }

    return 0;
}
