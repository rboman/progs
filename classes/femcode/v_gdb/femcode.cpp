#include "femcode.h"
#ifdef __GNUC__
#include <fenv.h>
#endif

int main() {
   #ifdef __GNUC__
   feenableexcept(-1);
   #endif
   Node n1(1, 1.0, 1.0, 1.0);
   printNode(n1);
   scaleNode(n1, 0.0);
   printNode(n1);
   return 0;
}
