
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

void printNode(Node &nod);
void translateNode(Node &nod, double tx);
