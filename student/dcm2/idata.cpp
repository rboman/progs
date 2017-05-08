//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//                         MODIFICATION DES DONNEES
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include <fstream.h>
#include <stdio.h>
#include <math.h>
#include <conio.h>

extern double densite, enverg, Mmoteurs, Mfuselage, MYoung;
extern double ep, c0, c1, T, F0, PREC, PREC2;
extern double np, np2, Nperiod, Nmodes;
extern void titre();

void param(char *texte, double *par)
{
      char entree[20];
      float prm = 0.0;
      printf("   %s [%f]=", texte, *par);
      gets(entree);
      sscanf(entree, "%f", &prm);
      if (fabs(prm) > 1E-10)
            *par = prm;
}

void input_data()
{
      clrscr();
      titre();
      cout << "\n-Dimensions de l'avion:\n";
      param("Densit�", &densite);
      param("Masse des moteurs", &Mmoteurs);
      param("Masse du fuselage", &Mfuselage);
      param("Module de Young", &MYoung);
      param("c0", &c0);
      param("c1", &c1);
      cout << "\n-2� partie:\n";
      param("P�riode T", &T);
      param("Force F0", &F0);
      cout << "\n-Donn�es relatives � Matlab:\n";
      param("Pas en x", &np);
      param("Pas en t", &np2);
      param("Nbre de p�riodes", &Nperiod);
      cout << "\n-Param�tres suppl�mentaires:\n";
      param("Envergure", &enverg);
      param("Epaisseur de l'aile", &ep);
      param("Nbre de modes F0", &Nmodes);
      param("Pr�cision sur les fr�q.", &PREC);
      param("Pr�cision sur les modes", &PREC2);
}
