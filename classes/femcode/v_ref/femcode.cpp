#include <stdio.h>

struct Node 
{  
  int num;
  double x;
  double y;
  double z;

  Node(int n=0, double px=0.0,
         double py=0.0, double pz=0.0)
  {
     num = n;
     x = px; y = py; z = pz;
  }
};


void translateNode(Node &nod, double tx) {
   nod.x += tx; 
}

void printNode(Node &nod) {
    printf("node #%d (%g,%g,%g)\n", 
           nod.num, nod.x, nod.y, nod.z);
}

int main() {
   Node n1(1, 0.0, 0.0, 0.0);
   printNode(n1);
   translateNode(n1, 1.0);
   printNode(n1);
   return 0;
}
