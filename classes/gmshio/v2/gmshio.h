// gmshio header

#include <cstring> // for "strncmp"
#include <cstdlib> // for "exit"
#include <cstdio>
#include <cmath>
#include <vector>
#include <map>

struct Node
{
    int num;        // label
    double x, y, z; // coordinates

    Node(int _num = 0, double _x = 0.0, double _y = 0.0, double _z = 0.0)
    {
        num = _num;
        x = _x;
        y = _y;
        z = _z;
    }
};

struct Element
{
    int num;                   // label
    int type;                  // type (3=quad; 5=hexa; ...)
    int region;                // physical group
    std::vector<Node *> nodes; // list of nodes

    Element(int _num = 0, int _type = 0, int _region = 0)
    {
        num = _num;
        type = _type;
        region = _region;
    }
};

void readMSH(const char *fileName, std::vector<Node *> &nodes,
             std::vector<Element *> &elements);

void writeMSH(const char *fileName, double time, int step,
              std::map<Node *, std::vector<double>> &solution);

void errorAndQuit(const char *msg, FILE *fp = NULL);
