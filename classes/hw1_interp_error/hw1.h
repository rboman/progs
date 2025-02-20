#ifndef HW1_H
#define HW1_H

struct GroupEntities;
struct Entity;
struct ElemPrp;
struct PhysicalGroups;
struct Nodes;
struct ElementData;

extern int verbosity;
// 0: silent except for fatal errors, 1: +errors, 2: +warnings, 3: +direct, 4: +information, 5: +status, 99: +debug
// use as: "if (verbosity>VERB::INFO) ..."
enum VERB
{
    FATAL_ERROR = -1,
    ERROR = 0,
    WARNING = 1,
    DIRECT = 2,
    INFO = 3,
    STATUS = 4,
    DEBUG = 98
};



#endif //HW1_H