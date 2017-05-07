// Class Base_Poly
// @ Igor KLAPKA - LTAS
// Novembre 94
//
// Comments:
//
#include "vararray.h"

#ifndef _BASE_POL_H
#define _BASE_POL_H
#include "polynome.h"

#define max_pol 20

struct Masses
{double masse;
 double x;
};

class Base_Poly :public Vararray<Polynome>  //Base de Polynome symetriques f(x)=f(-x)
{typedef short unsigned int indice;

	     indice taille;
	     double **K,young,l;
            Masses* MsX;
	   Polynome I, m;
 Vararray<Polynome> ddBase;

	       void build_k();

public:

 Base_Poly( Masses* _MsX,
	   Polynome _I,
           Polynome _m,
	     double _young ,
	     double _envergure ,
	   Polynome &P  ):Vararray<Polynome>(max_pol)

  {    young=_young;    //E    N/m2
         MsX=_MsX;
	   I=_I;        //I(x) m4
	   m=_m;        //m(x) kg/m
	   l=_envergure;

  (*this)[0]=P;
	   K=NULL;
  if( P.donne_degre( ) < 2)
   { Polynome vide(0); 
     ddBase[0]=vide;}
  else 
     ddBase[0]= ( P.derive() ).derive();
 
      taille=0; build_k();
      taille=1;
   };

 indice donne_taille() {return taille;};
 double ** ajoute_suivant();
 void affiche_K(int);

friend
 ostream &operator << (ostream & outp,Base_Poly &bp);

};
#endif
