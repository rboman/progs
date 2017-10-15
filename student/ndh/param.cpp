/*********************************************************************
 *                                                                   *
 *	      Travail N.D.H. : Eléments aux frontiéres               *
 *            ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   	     *
 *	      Version C++    derniére modif.: 10.12.96               *
 *                                                                   *
 *********************************************************************
 *  Programme : PARAM.CPP  (routines de modification de paramétres)  *
 *********************************************************************/

#include "ndh.h"

//--------------------------------------------------------------------
// Routine d'introduction d'un float au clavier
//--------------------------------------------------------------------

void param(char *texte, float *par)
{
      char entree[20];
      float prm = 0.0;
      printf("  %s [%f] =", texte, *par);
      gets(entree);
      sscanf(entree, "%f", &prm);
      if (fabs(prm) > 1E-10)
            *par = prm;
}

//--------------------------------------------------------------------
// Routine d'introduction d'un integer au clavier
//--------------------------------------------------------------------

void param2(char *texte, int *par)
{
      char entree[20];
      int prm = 0.0;
      printf("  %s [%d] =", texte, *par);
      gets(entree);
      sscanf(entree, "%d", &prm);
      if (abs(prm) > 0)
            *par = prm;
}

//--------------------------------------------------------------------
// Routine de modification des paramétres
//--------------------------------------------------------------------

void input_data()
{
      void destroy_vectors(), create_vectors(), define_geometry();
      void param(char *, float *), param2(char *, int *), titre();
      char entree[20];
      int j;

      clrscr();
      titre();
      param2("Probléme (1=cercle,2=carré,3=autre)", &probleme);
      param("Beta", &beta);
      param("k", &k);
      if (probleme == 1)
            param("Rayon", &R);
      else
            param("Coté", &a);
      param2("Nbre d'éléments aux frontiéres", &N);
      if (probleme == 2) // Le nbre d'élém. doit étre un multiple de 4.
      {
            j = N / 4;
            N = 4 * j;
      } // si le probléme est le carré.
      if (N < 2)
            N = 20;
      param2("Nbre de pas d'intégration par élément", &istep);
      if (istep < 2)
            istep = 5;
      if (probleme == 1)
            zoom = 200.0 / R;
      else
            zoom = 200.0 / a;
      param2("Type d'intégration (1=trapéze,2=Simpson,...,6=Weddle)", &ideg);
      if ((ideg < 1) || (ideg > 6))
            ideg = 1;
      j = istep / ideg;
      istep = j * ideg; // le nbre d'intervalles d'intégr.
      if (istep == 0)
            istep = ideg; // doit étre un mult. de 'ideg'.
      param2("Densité de visualisation", &density);
      param2("Maillage (1=on 2=off)", &maillag);
      param2("White Bg (1=on 2=off)", &whitebg);

      // Re-dimensionement des tableaux:
      destroy_vectors();
      create_vectors();
      define_geometry();
}