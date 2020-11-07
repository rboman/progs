#include "femcode.h"

int main() {
   Node n1(1, 0.0, 0.0, 0.0);
   printNode(n1);
   translateNode(n1, 1.0);
   printNode(n1);
   return 0;
}
