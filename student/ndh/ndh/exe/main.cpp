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

#include "BemSolver.h"

void main()
{
    // Initialisation des variables

    BemSolver ndh;

    ndh.create_vectors();
    ndh.define_geometry();

    // Menu
    int exit = 0;
    while (exit == 0)
    {
        clrscr();
        titre();
        std::cout << "\n\nProblème courant :";
        if (ndh.probleme == 1)
            std::cout << " CERCLE de rayon a";
        else if (ndh.probleme == 2)
            std::cout << " CARRE de côté a";
        else
            std::cout << "QUELCONQUE";
        std::cout << "\n\n\t [1]  Lancer le calcul complet.";
        std::cout << "\n\t [2]  Lancer le calcul rapide.";
        std::cout << "\n\t [3]  Paramètres.";
        std::cout << "\n\t [4]  Charger fichier données.";
        std::cout << "\n\t [5]  Visualisation graphique.";
        std::cout << "\n\t [6]  Evaluation de la solution analytique.";
        std::cout << "\n\t [7]  Sauvegarde vers MATLAB";
        std::cout << "\n\t [0]  Quitter.";
        std::cout << "\n\n\n\nFLOPS     : non disponible";
        //std::cout << "\nTemps CPU : " << (double)(time2 - time1) / CLK_TCK << " sec.";
        std::cout << "\n\nChoix\?+<ENTER>: ";
        
        int choix;
        std::cin >> choix;

        switch (choix)
        {
        case 1:
        {
            ndh.type = 1;
            ndh.full_calcul();
        }
        break;
        case 2:
        {
            ndh.type = 2;
            ndh.full_calcul();
        }
        break;
        case 3:
            ndh.input_data();
            break;
        case 4:
            ndh.load_data();
            break;
        case 5:
            visu();
            break;
        case 6:
            ndh.eval_Texact();
            break;
        case 7:
            ndh.save_Mfile();
            break;
        case 0:
        default:
            exit = 1;
        }
    }
    //clrscr();
}
