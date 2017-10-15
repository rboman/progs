//   Copyright 1996-2017 Romain Boman
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

#include "elmfr.h"
#include "iofun.h"

//--------------------------------------------------------------------
// Récupération d'un fichier de donnée (chargement)
// (attention : pas de vérification de l'existence du fichier!)
//--------------------------------------------------------------------

void load_data()
{
      char nom_fich[50];
      int i;
      //void titre(), destroy_vectors(), create_vectors();

      //clrscr();
      titre();
      range = N;
      probleme = 3;
      std::cout << "\nNom du fichier (.DAT) :";
      //gets(nom_fich);
      std::ifstream fich(nom_fich, std::ios::in);
      fich >> N; // Lecture du nombre d'éléments.
      fich >> zoom;
      destroy_vectors(); // Dimensionnement des tableaux
      create_vectors();  // en conséquence.
      for (i = 0; i <= N; i++)
      {
            fich >> xf[i];
            fich >> yf[i];
      }
      for (i = 0; i < N; i++)
      {
            xel[i] = (xf[i] + xf[i + 1]) / 2;
            yel[i] = (yf[i] + yf[i + 1]) / 2;
      }
      fich.close();
      calcul = 0;
}

//--------------------------------------------------------------------
// Sauvegarde des résultats dans un fichier MATLAB (*.M)
//--------------------------------------------------------------------

void save_Mfile()
{
      char nom_fich[50];
      float xb, yb;
      int i1, j1;
      void titre();

      //clrscr();
      titre();
      std::cout << "\nNom du fichier (.M) :";
      //gets(nom_fich);
      std::ofstream fich(nom_fich, std::ios::out);
      fich << "probleme =" << probleme << ';';
      fich << "\ndensity =" << density << ';';
      fich << "\nrange =" << range << ';';
      fich << "\nistep =" << istep << ';';
      fich << "\nN =" << N << ';';
      fich << "\nideg =" << ideg << ';';
      fich << "\ncpu =" << (double)(time2 - time1) / CLK_TCK << ';';
      fich << "\nTmin =" << Tmin << ';';
      fich << "\nTmax =" << Tmax << ';';
      for (i1 = 0; i1 < density; i1++)
            for (j1 = 0; j1 < range; j1++)
            {
                  if ((probleme == 1) && (range == 1))
                  {
                        xb = R / (density)*i1;
                        yb = 0.0;
                  }
                  else if (cartesien == 1)
                  {
                        xb = a / density * i1;
                        yb = a / range * j1;
                  }
                  else
                  {
                        xb = xel[j1] / density * i1 + (xel[j1] / density) / 2.0;
                        yb = yel[j1] / density * i1 + (yel[j1] / density) / 2.0;
                  }
                  fich << "\nxb(" << i1 + 1 << "," << j1 + 1 << ")=" << xb << ";";
                  fich << "\nyb(" << i1 + 1 << "," << j1 + 1 << ")=" << yb << ";";
                  fich << "\nT(" << i1 + 1 << "," << j1 + 1 << ")=" << T[i1][j1] << ";";
            }
      if ((probleme == 1) && (range == 1)) // Commandes de visualisation
            fich << "\nplot(xb,T); grid;";
      else
            fich << "\nmesh(xb,yb,T); grid;";
      fich.close();
}
