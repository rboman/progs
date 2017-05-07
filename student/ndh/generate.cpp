/*********************************************************************
 *                                                                   *
 *	      Travail N.D.H. : El‚ments aux frontiŠres               *
 *            ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   	     *
 *	      Version C++    derniŠre modif.: 30.11.96               *
 *                                                                   *
 *********************************************************************
 *  Programme : GENERATE.CPP  (g‚n‚rateur de contours)               *
 *********************************************************************/

#include "elmfr.h"

//--------------------------------------------------------------------
// Routine de g‚n‚ration d'une g‚om‚trie donn‚e et sauvegarde
// dans un fichier *.DAT
//--------------------------------------------------------------------

void tester()
{  int i; char nom_fich[50];
   void vector(float*, float, float, int);
   void create_vectors(),visu();

   R    = 1;
   N    = 50;
   zoom = 200.0/R;
   pi   = 4*atan(1);
   range=N;
   probleme=3;
   create_vectors();

   // G‚n‚ration:
   vector(alpha,0.0,(2*pi)/N,N+1);
   for(i=0; i<N+1; i++)
   {  xf[i]=(R-0.2+0.2*cos(3*alpha[i]))*cos(alpha[i]);
      yf[i]=(R-0.2+0.2*cos(3*alpha[i])*0.01*(-alpha[i]*alpha[i]*alpha[i]+2*pi))*sin(alpha[i]);
   }
   for(i=0; i<N; i++)
   {  xel[i]=(xf[i]+xf[i+1])/2;
      yel[i]=(yf[i]+yf[i+1])/2; }
   // Sortie vers fichier.DAT
   cout << "\nNom du fichier (.DAT) :";
   gets(nom_fich);
   ofstream fich(nom_fich, ios::out);
   fich << N;
   fich << "\n"<< zoom;
   for(i=0;i<=N;i++)
   {  fich << "\n"<< xf[i];
      fich << "\n"<< yf[i];
   }
   fich.close();
   calcul=0; visu();
}

void main()
{
   tester();
   cout << "\nSAUVEGARDE Ok...";
}
