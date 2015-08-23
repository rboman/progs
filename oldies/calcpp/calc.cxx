#include "expMath.hxx"

int main(int argc, char **argv)
{
  if(argc==2)
    {
      expMath A(argv[1]);
      float val;
      val=A.eval();
      std::cout << "Valeur = " << val << '\n';
      return 0;
    }
  else
    {
      std::cout << "\nPetite calculette.\n";
      std::cout << "\nUsage : " << argv[0] << " 'expr.'\n\n";
      return 0;
    }
}
