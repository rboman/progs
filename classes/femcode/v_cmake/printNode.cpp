#include "femcode.h"
#include <stdio.h>

void printNode(Node &nod) {
    printf("node #%d (%g,%g,%g)\n", 
           nod.num, nod.x, nod.y, nod.z);
}
