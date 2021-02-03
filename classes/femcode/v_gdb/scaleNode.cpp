#include "femcode.h"

void scaleNode(Node &nod, double s) {
    nod.x /= s;
    nod.y /= s;
    nod.z /= s;
}
