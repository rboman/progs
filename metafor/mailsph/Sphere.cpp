#include "Sphere.h"
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <vtkSmartPointer.h>
#include <vtkUnstructuredGrid.h>
#include <vtkReflectionFilter.h>
#include <vtkExtractUnstructuredGrid.h>
#include <vtkIncrementalOctreePointLocator.h>

Sphere::Sphere() : Mesh()
{
}

void Sphere::build()
{

	int nbe, nbc, noe_ini, maille_ini, mat1, loi1, mat2, loi2, taille, taille1, taille2,
		taille3, taille4, i, j, k, l, ****tab, *liste, **status, no, c, face, addon,
		n[4], s, cote, noe1, noe2, noe, louc, noe0, l_ini, l_fin, c_ini, c_fin, nint,
		maille, noe3, noe4, noe5, noe6, noe7, noe8, compteur, dao, couche, nbnoe,
		type1, type2, n1, n2, out, don[10][2], level0, level1, level2, out2, nop,
		sphere_creuse, ***cube, ep, ep_fin, ep_ini, nn[4], dim_status, nosph;

	double rint, rext, centre[3], r[3], **coord, xyz[3], alpha, ray, beta;

	FILE *fp_out;

	fp_out = fopen("out.dat", "w");

	fprintf(fp_out, ".DEL.*\n");

	// PARAMETRES

	sphere_creuse = 1; // 1 si creuse, 0 si pleine
	rint = 12.*5;		   // rayon interne si creuse, demi-diagonale du cube central si pleine
	rext = 20.*5;		   // rayon externe
	centre[0] = 0.0;   // coor x du centre
	centre[1] = 200.0;   // coor y du centre
	centre[2] = 0.0;   // coor z du centre
	r[0] = -1.0;	   // coor x du reducteur
	r[1] = -1.0;	   // coor y du reducteur
	r[2] = -1.0;	   // coor z du reducteur
	nbe = 5;		   // nombre d elements sur 1/6 d un m�ridien
	nbc = 2;		   // nombre d elements sur l epaisseur
	noe_ini = 0;
	maille_ini = 0;
	nosph = 1;

	// face externe

	mat1 = 1;
	loi1 = 1;
	type1 = 0; // -1 pas contact, 0 si rigide, 1 si defo-defo

	// face interne

	if (sphere_creuse == 1)
	{
		mat2 = 2;
		loi2 = 2;
		type2 = 1; // -1 pas contact, 0 si rigide, 1 si defo-defo
	}
	else
	{
		type2 = -1;
	}

	// FIN DES PARAMETRES

	// inversion des rayons pour rext > rint

	if (rext < rint)
	{
		ray = rext;
		rext = rint;
		rint = ray;
		ray = 0;
	}

	// Correction du nbe si tronque et impair

	if (r[0] != 0 || r[1] != 0 || r[2] != 0)
	{
		//     if(nbe % 2 != 0){
		nbe = 2 * nbe;
		//     }
	}

	if (type1 == 1)
	{
		mat1 = -mat1;
	}
	if (type2 == 1)
	{
		mat2 = -mat2;
	}

	// allocation du tableau tab(couche,face,lig,col)

	taille1 = (nbc + 1);
	tab = (int ****)calloc(taille1, sizeof(int ***));
	for (j = 0; j < taille1; j++)
	{
		taille2 = 6;
		tab[j] = (int ***)calloc(taille2, sizeof(int **));
		for (k = 0; k < taille2; k++)
		{
			taille3 = (nbe + 1);
			tab[j][k] = (int **)calloc(taille3, sizeof(int *));
			for (l = 0; l < taille3; l++)
			{
				taille4 = (nbe + 1);
				tab[j][k][l] = (int *)calloc(taille4, sizeof(int));
			}
		}
	}

	// allocation du tableau cube(couche,lig,col)

	if (sphere_creuse == 0)
	{
		taille1 = (nbe + 1);
		cube = (int ***)calloc(taille1, sizeof(int **));
		for (j = 0; j < taille1; j++)
		{
			taille2 = (nbe + 1);
			cube[j] = (int **)calloc(taille2, sizeof(int *));
			for (k = 0; k < taille2; k++)
			{
				taille3 = (nbe + 1);
				cube[j][k] = (int *)calloc(taille3, sizeof(int));
			}
		}
	}

	// allocation du tableau coord(numint, xyz)

	taille = (6 * nbe * nbe + 2) * (nbc + 1);
	if (sphere_creuse == 0)
	{
		taille = taille + (nbe - 1) * (nbe - 1) * (nbe - 1);
	}

	coord = (double **)calloc(taille, sizeof(double *));
	for (j = 0; j < taille; j++)
	{
		coord[j] = (double *)calloc(3, sizeof(double));
	}

	//  allocation du vecteur liste(numint)=numdao

	taille = (6 * nbe * nbe + 2) * (nbc + 1);
	if (sphere_creuse == 0)
	{
		taille = taille + (nbe - 1) * (nbe - 1) * (nbe - 1);
	}
	liste = (int *)calloc(taille, sizeof(int));

	// allocation du tableau status(face,l/c)

	dim_status = 6;
	if (sphere_creuse == 0)
	{
		dim_status = dim_status + 1;
	}
	status = (int **)calloc(dim_status, sizeof(int *));
	for (j = 0; j < dim_status; j++)
	{
		taille2 = 2;
		if (j == 6)
		{
			taille2 = taille2 + 1;
		}
		status[j] = (int *)calloc(taille2, sizeof(int));
	}

	// fin des allocations

	// remplissage de tab

	// couche 0 (exterieure)

	no = -1;

	for (face = 0; face < 6; face++)
	{
		for (l = 0; l < nbe + 1; l++)
		{
			for (c = 0; c < nbe + 1; c++)
			{
				if (face == 1 && c == 0)
				{
					tab[0][face][l][c] = tab[0][face - 1][l][nbe];
				}
				else if (face == 2 && c == 0)
				{
					tab[0][face][l][c] = tab[0][face - 1][l][nbe];
				}
				else if (face == 3 && c == 0)
				{
					tab[0][face][l][c] = tab[0][face - 1][l][nbe];
				}
				else if (face == 3 && c == nbe)
				{
					tab[0][face][l][c] = tab[0][face - 3][l][0];
				}
				else if (face == 4 && c == 0)
				{
					tab[0][face][l][c] = tab[0][face - 3][0][l];
				}
				else if (face == 4 && c == nbe)
				{
					tab[0][face][l][c] = tab[0][face - 1][0][nbe - l];
				}
				else if (face == 4 && l == 0)
				{
					tab[0][face][l][c] = tab[0][face - 4][0][nbe - c];
				}
				else if (face == 4 && l == nbe)
				{
					tab[0][face][l][c] = tab[0][face - 2][0][c];
				}
				else if (face == 5 && c == 0)
				{
					tab[0][face][l][c] = tab[0][face - 2][nbe][nbe - l];
				}
				else if (face == 5 && c == nbe)
				{
					tab[0][face][l][c] = tab[0][face - 4][nbe][l];
				}
				else if (face == 5 && l == 0)
				{
					tab[0][face][l][c] = tab[0][face - 5][nbe][c];
				}
				else if (face == 5 && l == nbe)
				{
					tab[0][face][l][c] = tab[0][face - 3][nbe][nbe - c];
				}
				else
				{
					no = no + 1;
					tab[0][face][l][c] = no;
				}
			}
		}
	}

	// On en d�duit toutes les couches

	for (couche = 1; couche <= nbc; couche++)
	{
		addon = couche * (6 * nbe * nbe + 2);
		for (face = 0; face < 6; face++)
		{
			for (l = 0; l < nbe + 1; l++)
			{
				for (c = 0; c < nbe + 1; c++)
				{
					tab[couche][face][l][c] = tab[0][face][l][c] + addon;
				}
			}
		}
	}

	// remplissage de cube

	if (sphere_creuse == 0)
	{
		no = no + nbc * (6 * nbe * nbe + 2);
		for (ep = 0; ep < nbe + 1; ep++)
		{
			for (l = 0; l < nbe + 1; l++)
			{
				for (c = 0; c < nbe + 1; c++)
				{
					if (ep == 0)
					{
						cube[ep][l][c] = tab[nbc][0][l][c];
					}
					else if (ep == nbe)
					{
						cube[ep][l][c] = tab[nbc][2][l][nbe - c];
					}
					else if (l == 0)
					{
						cube[ep][l][c] = tab[nbc][4][ep][nbe - c];
					}
					else if (l == nbe)
					{
						cube[ep][l][c] = tab[nbc][5][ep][c];
					}
					else if (c == 0)
					{
						cube[ep][l][c] = tab[nbc][3][l][nbe - ep];
					}
					else if (c == nbe)
					{
						cube[ep][l][c] = tab[nbc][1][l][ep];
					}
					else
					{
						no = no + 1;
						cube[ep][l][c] = no;
					}
				}
			}
		}
	}

	// Calcul des coordonn�es

	// La face 0

	// Les coins

	// couche exterieure

	n[0] = tab[0][0][0][0];
	n[1] = tab[0][0][0][nbe];
	n[2] = tab[0][0][nbe][nbe];
	n[3] = tab[0][0][nbe][0];

	ray = rext / sqrt(3.);

	for (i = 0; i < 3; i++)
	{
		for (j = 0; j < 4; j++)
		{
			if (((j == 1 || j == 2) && i == 0) || ((j == 2 || j == 3) && i == 2))
			{
				s = -1;
			}
			else
			{
				s = 1;
			}

			coord[n[j]][i] = s * ray;
		}
	}

	// couche interieure

	nn[0] = tab[nbc][0][0][0];
	nn[1] = tab[nbc][0][0][nbe];
	nn[2] = tab[nbc][0][nbe][nbe];
	nn[3] = tab[nbc][0][nbe][0];
	ray = rint / rext;
	for (i = 0; i < 3; i++)
	{
		for (j = 0; j < 4; j++)
		{
			coord[nn[j]][i] = coord[n[j]][i] * ray;
		}
	}

	// Les cotes

	// couche exterieure

	for (cote = 0; cote < 4; cote++)
	{

		if (cote == 0)
		{
			l = 0;
		}
		if (cote == 1)
		{
			c = nbe;
		}
		if (cote == 2)
		{
			l = nbe;
		}
		if (cote == 3)
		{
			c = 0;
		}

		if (cote == 0 || cote == 3)
		{
			noe1 = n[0];
		}
		else if (cote == 1)
		{
			noe1 = n[1];
		}
		else if (cote == 2)
		{
			noe1 = n[3];
		}

		if (cote == 1 || cote == 2)
		{
			noe2 = n[2];
		}
		else if (cote == 0)
		{
			noe2 = n[1];
		}
		else if (cote == 3)
		{
			noe2 = n[3];
		}

		if (cote == 0 || cote == 2)
		{
			for (c = 1; c < nbe; c++)
			{
				noe = tab[0][0][l][c];
				prog1(coord, noe1, noe2, c, nbe, xyz, rext);
				for (i = 0; i < 3; i++)
				{
					coord[noe][i] = xyz[i];
				}
			}
		}
		else if (cote == 1 || cote == 3)
		{
			for (l = 1; l < nbe; l++)
			{
				noe = tab[0][0][l][c];
				prog1(coord, noe1, noe2, l, nbe, xyz, rext);
				for (i = 0; i < 3; i++)
				{
					coord[noe][i] = xyz[i];
				}
			}
		}
	}

	// Couche interieure

	if (sphere_creuse == 0)
	{
		for (cote = 0; cote < 4; cote++)
		{

			if (cote == 0)
			{
				l = 0;
			}
			if (cote == 1)
			{
				c = nbe;
			}
			if (cote == 2)
			{
				l = nbe;
			}
			if (cote == 3)
			{
				c = 0;
			}

			if (cote == 0 || cote == 3)
			{
				noe1 = nn[0];
			}
			else if (cote == 1)
			{
				noe1 = nn[1];
			}
			else if (cote == 2)
			{
				noe1 = nn[3];
			}

			if (cote == 1 || cote == 2)
			{
				noe2 = nn[2];
			}
			else if (cote == 0)
			{
				noe2 = nn[1];
			}
			else if (cote == 3)
			{
				noe2 = nn[3];
			}

			if (cote == 0 || cote == 2)
			{
				for (c = 1; c < nbe; c++)
				{
					noe = tab[nbc][0][l][c];
					for (i = 0; i < 3; i++)
					{
						coord[noe][i] = coord[noe1][i] + (c * 1.) / (nbe * 1.) * (coord[noe2][i] - coord[noe1][i]);
					}
				}
			}
			else if (cote == 1 || cote == 3)
			{
				for (l = 1; l < nbe; l++)
				{
					noe = tab[nbc][0][l][c];
					for (i = 0; i < 3; i++)
					{
						coord[noe][i] = coord[noe1][i] + (l * 1.) / (nbe * 1.) * (coord[noe2][i] - coord[noe1][i]);
					}
				}
			}
		}
	}

	// L int�rieur

	// Couche exterieure

	for (l = 1; l < nbe; l++)
	{
		noe1 = tab[0][0][l][0];
		noe2 = tab[0][0][l][nbe];

		for (c = 1; c < nbe; c++)
		{
			noe = tab[0][0][l][c];
			prog1(coord, noe1, noe2, c, nbe, xyz, rext);
			for (i = 0; i < 3; i++)
			{
				coord[noe][i] = xyz[i];
			}
		}
	}

	// couche interieure

	if (sphere_creuse == 0)
	{
		for (l = 1; l < nbe; l++)
		{
			noe1 = tab[nbc][0][l][0];
			noe2 = tab[nbc][0][l][nbe];

			for (c = 1; c < nbe; c++)
			{
				noe = tab[nbc][0][l][c];
				for (i = 0; i < 3; i++)
				{
					coord[noe][i] = coord[noe1][i] + (c * 1.) / (nbe * 1.) * (coord[noe2][i] - coord[noe1][i]);
				}
			}
		}
	}
	else if (sphere_creuse == 1)
	{
		for (l = 0; l < nbe + 1; l++)
		{
			for (c = 0; c < nbe + 1; c++)
			{
				noe = tab[nbc][0][l][c];
				noe0 = tab[0][0][l][c];
				ray = rint / rext;
				for (i = 0; i < 3; i++)
				{
					coord[noe][i] = coord[noe0][i] * ray;
				}
			}
		}
	}

	// Toutes les couches de la face 0

	if (nbc > 1)
	{
		for (couche = 1; couche < nbc; couche++)
		{
			for (l = 0; l < nbe + 1; l++)
			{
				for (c = 0; c < nbe + 1; c++)
				{
					noe = tab[couche][0][l][c];
					noe1 = tab[0][0][l][c];
					noe2 = tab[nbc][0][l][c];
					for (i = 0; i < 3; i++)
					{
						coord[noe][i] = coord[noe1][i] + (couche * 1.) / (nbc * 1.) * (coord[noe2][i] - coord[noe1][i]);
					}
				}
			}
		}
	}

	// generation des faces 1 a 3

	for (face = 1; face < 4; face++)
	{
		c_ini = 1;
		c_fin = nbe + 1;
		if (face == 3)
		{
			c_fin = nbe;
		}
		for (couche = 0; couche < nbc + 1; couche++)
		{
			for (l = 0; l < nbe + 1; l++)
			{
				for (c = c_ini; c < c_fin; c++)
				{
					noe = tab[couche][face][l][c];
					noe0 = tab[couche][face - 1][l][c];
					coord[noe][0] = -coord[noe0][1];
					coord[noe][1] = coord[noe0][0];
					coord[noe][2] = coord[noe0][2];
				}
			}
		}
	}

	// generation de la face 4

	face = 4;
	for (couche = 0; couche < nbc + 1; couche++)
	{
		for (l = 1; l < nbe; l++)
		{
			for (c = 1; c < nbe; c++)
			{
				noe = tab[couche][face][l][c];
				noe0 = tab[couche][2][l][c];
				coord[noe][0] = coord[noe0][0];
				coord[noe][1] = coord[noe0][2];
				coord[noe][2] = -coord[noe0][1];
			}
		}
	}

	// generation de la face 5

	face = 5;
	for (couche = 0; couche < nbc + 1; couche++)
	{
		for (l = 1; l < nbe; l++)
		{
			for (c = 1; c < nbe; c++)
			{
				noe = tab[couche][face][l][c];
				noe0 = tab[couche][0][l][c];
				coord[noe][0] = coord[noe0][0];
				coord[noe][1] = coord[noe0][2];
				coord[noe][2] = -coord[noe0][1];
			}
		}
	}

	// generation du cube central

	if (sphere_creuse == 0)
	{
		if (nbe > 1)
		{
			for (ep = 1; ep < nbe; ep++)
			{
				for (l = 1; l < nbe; l++)
				{
					for (c = 1; c < nbe; c++)
					{
						noe = cube[ep][l][c];
						ray = rint / sqrt(3.);
						coord[noe][0] = ray * (1 - ((2 * c) * 1.) / (nbe * 1.));
						coord[noe][1] = ray * (1 - ((2 * ep) * 1.) / (nbe * 1.));
						coord[noe][2] = ray * (1 - ((2 * l) * 1.) / (nbe * 1.));
					}
				}
			}
		}
	}

	// mise � jour des positions avec le centre centre[i]

	for (i = 0; i < 3; i++)
	{
		taille = (1 + nbc) * (6 * nbe * nbe + 2);
		if (sphere_creuse == 0)
		{
			taille = taille + (nbe - 1) * (nbe - 1) * (nbe - 1);
		}
		for (j = 0; j < taille; j++)
		{
			coord[j][i] = coord[j][i] + centre[i];
		}
	}

	// fin du remplissage du tableau des coord.

	// prise en compte du decoupage

	for (i = 0; i < dim_status; i++)
	{
		for (j = 0; j < 2; j++)
		{
			status[i][j] = 0;
		}
		if (i == 6)
		{
			status[i][2] = 0;
		}
	}
	if (r[0] != 0 || r[1] != 0 || r[2] != 0)
	{
		if (r[0] > 0.)
		{
			status[0][0] = 1;
			status[1][0] = 3;
			status[1][1] = 3;
			status[2][0] = 2;
			status[4][0] = 2;
			status[5][0] = 1;
			if (sphere_creuse == 0)
			{
				status[6][0] = 1;
			}
		}
		else if (r[0] < 0.)
		{
			status[0][0] = 2;
			status[2][0] = 1;
			status[3][0] = 3;
			status[3][1] = 3;
			status[4][0] = 1;
			status[5][0] = 2;
			if (sphere_creuse == 0)
			{
				status[6][0] = 2;
			}
		}
		if (r[1] > 0.)
		{
			if (status[1][0] != 3)
			{
				status[1][0] = 1;
			}
			status[2][0] = 3;
			status[2][1] = 3;
			if (status[3][0] != 3)
			{
				status[3][0] = 2;
			}
			status[4][1] = 1;
			status[5][1] = 1;
			if (sphere_creuse == 0)
			{
				status[6][2] = 1;
			}
		}
		else if (r[1] < 0.)
		{
			status[0][0] = 3;
			status[0][1] = 3;
			if (status[1][0] != 3)
			{
				status[1][0] = 2;
			}
			if (status[3][0] != 3)
			{
				status[3][0] = 1;
			}
			status[4][1] = 2;
			status[5][1] = 2;
			if (sphere_creuse == 0)
			{
				status[6][2] = 2;
			}
		}
		if (r[2] > 0.)
		{
			if (status[0][1] != 3)
			{
				status[0][1] = 1;
			}
			if (status[1][1] != 3)
			{
				status[1][1] = 1;
			}
			if (status[2][1] != 3)
			{
				status[2][1] = 1;
			}
			if (status[3][1] != 3)
			{
				status[3][1] = 1;
			}
			status[5][0] = 3;
			status[5][1] = 3;
			if (sphere_creuse == 0)
			{
				status[6][1] = 1;
			}
		}
		else if (r[2] < 0.)
		{
			if (status[0][1] != 3)
			{
				status[0][1] = 2;
			}
			if (status[1][1] != 3)
			{
				status[1][1] = 2;
			}
			if (status[2][1] != 3)
			{
				status[2][1] = 2;
			}
			if (status[3][1] != 3)
			{
				status[3][1] = 2;
			}
			status[4][0] = 3;
			status[4][1] = 3;
			if (sphere_creuse == 0)
			{
				status[6][1] = 2;
			}
		}
	}

	//  impression des donn�es du probl�me

	if (sphere_creuse == 1)
	{
		fprintf(fp_out, "! Sphere %2d Creuse \n\n", nosph);
	}
	if (sphere_creuse == 0)
	{
		fprintf(fp_out, "! Sphere %2d Pleine \n\n", nosph);
	}
	fprintf(fp_out, "abrev '/rext' '%15.8E' ! Rayon Exterieur \n", rext);
	if (sphere_creuse == 1)
	{
		fprintf(fp_out, "abrev '/rint' '%15.8E' ! Rayon Interieur \n", rint);
	}
	if (sphere_creuse == 0)
	{
		fprintf(fp_out, "abrev '/rap' '%15.8E' ! Rapport de la diagonale du cube central au rayon exterieur \n", rint);
	}

	fprintf(fp_out, "abrev '/xcentre' '%15.8E' ! Coordonnee X du centre \n", centre[0]);
	fprintf(fp_out, "abrev '/ycentre' '%15.8E' ! Coordonnee Y du centre \n", centre[0]);
	fprintf(fp_out, "abrev '/zcentre' '%15.8E' ! Coordonnee Z du centre \n", centre[0]);

	fprintf(fp_out, "! Coefficient reducteur: X '%15.8E' Y '%15.8E' Z '%15.8E' \n", r[0], r[1], r[2]);

	fprintf(fp_out, "abrev '/nbe' '%3d' ! Nombre d'elements sur un cote de facette \n", nbe);
	fprintf(fp_out, "abrev '/nbc' '%3d' ! Nombre de couches d'�l�ments (hors cube central si plein) \n", nbc);
	fprintf(fp_out, "abrev '/noeini' '%3d' ! Numero du noeud initial - 1\n", noe_ini);
	fprintf(fp_out, "abrev '/maiini' '%3d' ! Numero de la maille initiale - 1\n", maille_ini);
	if (type1 == 0)
	{
		fprintf(fp_out, "! Sa surface ext�rieure est la matrice de contact rigide numero %2d \n", mat1);
	}
	if (type1 == 1)
	{
		fprintf(fp_out, "! Sa surface ext�rieure est la matrice de contact deformable numero %2d \n", mat1);
	}
	if (type2 == 0)
	{
		fprintf(fp_out, "! Sa surface int�rieure est la matrice de contact rigide numero %2d \n", mat2);
	}
	if (type2 == 1)
	{
		fprintf(fp_out, "! Sa surface int�rieure est la matrice de contact deformable numero %2d \n", mat2);
	}
	fprintf(fp_out, "! Tout ceci pour la sphere numero %2d  \n", nosph);
	fprintf(fp_out, " \n");
	fprintf(fp_out, " \n");

	//  impression du .NOE

	fprintf(fp_out, "\n.NOEUD\n");

	// -VTK----------------------------------------------------------------------------------------------
	auto ugrid = vtkSmartPointer<vtkUnstructuredGrid>::New();
	auto points = vtkSmartPointer<vtkPoints>::New();
	ugrid->SetPoints(points);

	noe = noe_ini;

	for (couche = 0; couche < nbc + 1; couche++)
	{
		for (face = 0; face < 6; face++)
		{
			if (status[face][0] != 3)
			{
				l_ini = 0;
				l_fin = nbe;
				c_ini = 0;
				c_fin = nbe;

				if (status[face][0] == 1)
				{
					c_fin = nbe / 2;
				}
				else if (status[face][0] == 2)
				{
					c_ini = nbe / 2;
				}

				if (status[face][1] == 1)
				{
					l_fin = nbe / 2;
				}
				else if (status[face][1] == 2)
				{
					l_ini = nbe / 2;
				}

				for (l = l_ini; l <= l_fin; l++)
				{
					for (c = c_ini; c <= c_fin; c++)
					{
						nint = tab[couche][face][l][c];
						if (liste[nint] == 0)
						{
							noe = noe + 1;
							if (noe % 11111 == 0 || noe == 9999)
							{
								noe = noe + 1;
							}
							if (noe > 99998)
							{
								fprintf(fp_out, "Erreur, numero de noeuds trop grand pour BACON\n");
								throw std::runtime_error("bad parameters!");
							}
							fprintf(fp_out, "I %8d       X %15.8E    Y %15.8E    Z %15.8E\n",
									noe, coord[nint][0], coord[nint][1], coord[nint][2]);
							// VTK
							points->InsertPoint(noe, coord[nint][0], coord[nint][1], coord[nint][2]);

							liste[nint] = noe;
						}
					}
				}
			}
		}
	}
	if (sphere_creuse == 0)
	{
		face = 6;

		l_ini = 0;
		l_fin = nbe;
		c_ini = 0;
		c_fin = nbe;
		ep_ini = 0;
		ep_fin = nbe;

		if (status[face][0] == 1)
		{
			c_fin = nbe / 2;
		}
		else if (status[face][0] == 2)
		{
			c_ini = nbe / 2;
		}

		if (status[face][1] == 1)
		{
			l_fin = nbe / 2;
		}
		else if (status[face][1] == 2)
		{
			l_ini = nbe / 2;
		}

		if (status[face][2] == 1)
		{
			ep_fin = nbe / 2;
		}
		else if (status[face][2] == 2)
		{
			ep_ini = nbe / 2;
		}

		for (ep = ep_ini; ep <= ep_fin; ep++)
		{
			for (l = l_ini; l <= l_fin; l++)
			{
				for (c = c_ini; c <= c_fin; c++)
				{
					nint = cube[ep][l][c];
					if (liste[nint] == 0)
					{
						noe = noe + 1;
						if (noe % 11111 == 0 || noe == 9999)
						{
							noe = noe + 1;
						}
						if (noe > 99998)
						{
							fprintf(fp_out, "Erreur, numero de noeuds trop grand pour BACON\n");
							throw std::runtime_error("bad parameters!");
						}
						fprintf(fp_out, "I %8d       X %15.8E    Y %15.8E    Z %15.8E\n",
								noe, coord[nint][0], coord[nint][1], coord[nint][2]);
						// VTK
						points->InsertPoint(noe, coord[nint][0], coord[nint][1], coord[nint][2]);

						liste[nint] = noe;
					}
				}
			}
		}
	}

	// impression du .MAI

	fprintf(fp_out, "\n.MAI\n");
	maille = maille_ini;

	for (couche = 1; couche <= nbc; couche++)
	{
		for (face = 0; face < 6; face++)
		{
			if (status[face][0] != 3)
			{
				l_ini = 0;
				l_fin = nbe - 1;
				c_ini = 0;
				c_fin = nbe - 1;

				if (status[face][0] == 1)
				{
					c_fin = nbe / 2 - 1;
				}
				else if (status[face][0] == 2)
				{
					c_ini = nbe / 2;
				}

				if (status[face][1] == 1)
				{
					l_fin = nbe / 2 - 1;
				}
				else if (status[face][1] == 2)
				{
					l_ini = nbe / 2;
				}

				for (l = l_ini; l <= l_fin; l++)
				{
					for (c = c_ini; c <= c_fin; c++)
					{
						maille = maille + 1;
						noe1 = tab[couche - 1][face][l][c];
						noe2 = tab[couche - 1][face][l][c + 1];
						noe3 = tab[couche - 1][face][l + 1][c + 1];
						noe4 = tab[couche - 1][face][l + 1][c];
						noe5 = tab[couche][face][l][c];
						noe6 = tab[couche][face][l][c + 1];
						noe7 = tab[couche][face][l + 1][c + 1];
						noe8 = tab[couche][face][l + 1][c];
						fprintf(fp_out, "I %8d     N %6d %6d %6d %6d     0 %6d %6d %6d %6d ATT %2d\n", maille,
								liste[noe1], liste[noe2], liste[noe3], liste[noe4],
								liste[noe5], liste[noe6], liste[noe7], liste[noe8], face);

						insertvtkcell(ugrid, liste[noe1], liste[noe2], liste[noe3], liste[noe4],
									  liste[noe5], liste[noe6], liste[noe7], liste[noe8]);
					}
				}
			}
		}
	}
	if (sphere_creuse == 0)
	{
		face = 6;

		l_ini = 0;
		l_fin = nbe - 1;
		c_ini = 0;
		c_fin = nbe - 1;
		ep_ini = 0;
		ep_fin = nbe - 1;

		if (status[face][0] == 1)
		{
			c_fin = nbe / 2 - 1;
		}
		else if (status[face][0] == 2)
		{
			c_ini = nbe / 2;
		}

		if (status[face][1] == 1)
		{
			l_fin = nbe / 2 - 1;
		}
		else if (status[face][1] == 2)
		{
			l_ini = nbe / 2;
		}

		if (status[face][2] == 1)
		{
			ep_fin = nbe / 2 - 1;
		}
		else if (status[face][2] == 2)
		{
			ep_ini = nbe / 2;
		}

		for (ep = ep_ini; ep <= ep_fin; ep++)
		{
			for (l = l_ini; l <= l_fin; l++)
			{
				for (c = c_ini; c <= c_fin; c++)
				{
					maille = maille + 1;
					noe1 = cube[ep][l][c];
					noe2 = cube[ep][l][c + 1];
					noe3 = cube[ep][l + 1][c + 1];
					noe4 = cube[ep][l + 1][c];
					noe5 = cube[ep + 1][l][c];
					noe6 = cube[ep + 1][l][c + 1];
					noe7 = cube[ep + 1][l + 1][c + 1];
					noe8 = cube[ep + 1][l + 1][c];
					fprintf(fp_out, "I %8d     N %6d %6d %6d %6d     0 %6d %6d %6d %6d ATT %2d\n", maille,
							liste[noe1], liste[noe2], liste[noe3], liste[noe4],
							liste[noe5], liste[noe6], liste[noe7], liste[noe8], face);

					insertvtkcell(ugrid, liste[noe1], liste[noe2], liste[noe3], liste[noe4],
								  liste[noe5], liste[noe6], liste[noe7], liste[noe8]);
				}
			}
		}
	}

	//  impression du .MCO

	fprintf(fp_out, "\n.MCO\n\n");

	//   couche exterieure

	//   matrice rigide

	if (type1 == 0)
	{

		for (i = 0; i < 10; i++)
		{
			for (j = 0; j < 2; j++)
			{
				don[i][j] = 0;
			}
		}

		compteur = 0;
		for (i = 0; i < 3; i++)
		{
			if (r[i] != 0)
			{
				compteur = compteur + 1;
			}
		}
		if (compteur == 0)
		{
			nbnoe = 6 * nbe * nbe + 2;
		}
		else if (compteur == 1)
		{
			nbnoe = 3 * nbe * nbe + 2 * nbe + 1;
		}
		else if (compteur == 2)
		{
			nbnoe = 3 * nbe * nbe / 2 + 2 * nbe + 1;
		}
		else if (compteur == 3)
		{
			nbnoe = 3 * nbe * nbe / 4 + 3 * nbe / 2 + 1;
		}
		n1 = noe_ini + 1;
		n2 = n1 + nbnoe - 1;
		i = -1;
		out = 0;
		do
		{
			i = i + 1;
			if ((i == 0 && n1 < 9999) || (i > 0 && n1 < (i * 11111)))
			{
				don[i][0] = n1;
				level1 = i;
				out = 1;
			}
		} while (out == 0 && i < 10);
		out = 0;
		i = i - 1;
		do
		{
			i = i + 1;
			if ((i == 0 && n2 < 9999) || (i > 0 && n2 < (i * 11111)))
			{
				don[i][1] = n2;
				level2 = i;
				out = 1;
			}
			else
			{
				if (i == 0)
				{
					don[i][1] = 9998;
					don[i + 1][0] = 10000;
					n2 = n2 + 1;
				}
				else
				{
					don[i][1] = (i * 11111) - 1;
					don[i + 1][0] = (i * 11111) + 1;
					n2 = n2 + 1;
				}
			}
		} while (out == 0 && i < 10);
		for (i = level1; i <= level2; i++)
		{
			if (don[i][0] != 0)
			{
				fprintf(fp_out, "I %6d      J %6d    MAT %2d   LOI %2d \n",
						don[i][0], don[i][1], mat1, loi1);
			}
		}
		fprintf(fp_out, "\n");
	}

	//   matrice souple

	if (type1 == 1)
	{
		couche = 0;
		compteur = 0;

		for (face = 0; face < 6; face++)
		{
			if (status[face][0] != 3)
			{

				if (compteur > 0)
				{
					fprintf(fp_out, "I %6d \n", -4);
				}
				compteur = compteur + 1;

				l_ini = 0;
				l_fin = nbe;
				c_ini = 0;
				c_fin = nbe;

				if (status[face][0] == 1)
				{
					c_fin = nbe / 2;
				}
				else if (status[face][0] == 2)
				{
					c_ini = nbe / 2;
				}

				if (status[face][1] == 1)
				{
					l_fin = nbe / 2;
				}
				else if (status[face][1] == 2)
				{
					l_ini = nbe / 2;
				}

				for (l = l_ini; l <= l_fin; l++)
				{
					for (c = c_ini; c <= c_fin; c++)
					{
						noe = tab[couche][face][l][c];
						dao = liste[noe];
						nop = 0;
						if (c == 0 || c == nbe || l == 0 || l == nbe)
						{
							if (face == 0)
							{
								nop = -1;
							}
							else if (face == 1 || face == 2)
							{
								if (c == 0)
								{
									nop = -2;
								}
								else
								{
									nop = -1;
								}
							}
							else if (face == 3)
							{
								if (c == 0 || c == nbe)
								{
									nop = -2;
								}
								else
								{
									nop = -1;
								}
							}
							if (face == 4 || face == 5)
							{
								nop = -2;
							}
							fprintf(fp_out, "I %6d      MAT %3d   LOI %2d NOP %2d \n", dao, mat1, loi1, nop);
						}
						else
						{
							fprintf(fp_out, "I %6d      MAT %3d   LOI %2d \n", dao, mat1, loi1);
						}
					}
					if (l != l_fin)
					{
						fprintf(fp_out, "I %8d \n", -3);
					}
				}
			}
		}
		fprintf(fp_out, "\n");
	}

	//   couche interieure

	//   matrice rigide

	if (type2 == 0)
	{

		for (i = 0; i < 10; i++)
		{
			for (j = 0; j < 2; j++)
			{
				don[i][j] = 0;
			}
		}

		compteur = 0;
		for (i = 0; i < 3; i++)
		{
			if (r[i] != 0)
			{
				compteur = compteur + 1;
			}
		}
		if (compteur == 0)
		{
			nbnoe = 6 * nbe * nbe + 2;
		}
		else if (compteur == 1)
		{
			nbnoe = 3 * nbe * nbe + 2 * nbe + 1;
		}
		else if (compteur == 2)
		{
			nbnoe = 3 * nbe * nbe / 2 + 2 * nbe + 1;
		}
		else if (compteur == 3)
		{
			nbnoe = 3 * nbe * nbe / 4 + 3 * nbe / 2 + 1;
		}
		n1 = noe_ini + nbc * nbnoe + 1;
		n2 = n1 + nbc * nbnoe - 1;
		i = -1;
		out = 0;
		out2 = 0;
		do
		{
			i = i + 1;
			if (((i == 0 && (noe_ini + 1) < 9999) || (i > 0 && (noe_ini + 1) < (i * 11111))) && (out2 == 0))
			{
				level0 = i;
				out2 = 1;
			}
			if ((i == 0 && n1 < 9999) || (i > 0 && n1 < (i * 11111)))
			{
				level1 = i;
				n1 = n1 + i - level0;
				n2 = n2 + i - level0;
				don[i][0] = n1;
				out = 1;
			}
		} while (out == 0 && i < 10);
		out = 0;
		i = i - 1;
		do
		{
			i = i + 1;
			if ((i == 0 && n2 < 9999) || (i > 0 && n2 < (i * 11111)))
			{
				don[i][1] = n2;
				level2 = i;
				out = 1;
			}
			else
			{
				if (i == 0)
				{
					don[i][1] = 9998;
					don[i + 1][0] = 10000;
					n2 = n2 + 1;
				}
				else
				{
					don[i][1] = (i * 11111) - 1;
					don[i + 1][0] = (i * 11111) + 1;
					n2 = n2 + 1;
				}
			}
		} while (out == 0 && i < 10);
		for (i = level1; i <= level2; i++)
		{
			if (don[i][0] != 0)
			{
				fprintf(fp_out, "I %6d      J %6d    MAT %2d   LOI %2d \n",
						don[i][0], don[i][1], mat2, loi2);
			}
		}
		fprintf(fp_out, "\n");
	}

	//   matrice souple

	if (type2 == 1)
	{
		couche = nbc;
		compteur = 0;

		for (face = 0; face < 6; face++)
		{
			if (status[face][0] != 3)
			{

				if (compteur > 0)
				{
					fprintf(fp_out, "I %6d \n", -4);
				}
				compteur = compteur + 1;

				l_ini = 0;
				l_fin = nbe;
				c_ini = 0;
				c_fin = nbe;

				if (status[face][0] == 1)
				{
					c_fin = nbe / 2;
				}
				else if (status[face][0] == 2)
				{
					c_ini = nbe / 2;
				}

				if (status[face][1] == 1)
				{
					l_fin = nbe / 2;
				}
				else if (status[face][1] == 2)
				{
					l_ini = nbe / 2;
				}

				for (l = l_ini; l <= l_fin; l++)
				{
					for (c = c_ini; c <= c_fin; c++)
					{
						noe = tab[couche][face][(l_fin + l_ini) - l][c];
						dao = liste[noe];
						nop = 0;
						if (c == 0 || c == nbe || l == 0 || l == nbe)
						{
							if (face == 0)
							{
								nop = -1;
							}
							else if (face == 1 || face == 2)
							{
								if (c == 0)
								{
									nop = -2;
								}
								else
								{
									nop = -1;
								}
							}
							else if (face == 3)
							{
								if (c == 0 || c == nbe)
								{
									nop = -2;
								}
								else
								{
									nop = -1;
								}
							}
							if (face == 4 || face == 5)
							{
								nop = -2;
							}
							fprintf(fp_out, "I %6d      MAT %3d   LOI %2d NOP %2d \n", dao, mat2, loi2, nop);
						}
						else
						{
							fprintf(fp_out, "I %6d      MAT %3d   LOI %2d \n", dao, mat2, loi2);
						}
					}
					if (l != l_fin)
					{
						fprintf(fp_out, "I %6d \n", -3);
					}
				}
			}
		}
		fprintf(fp_out, "\n");
	}
	fprintf(fp_out, ".DES\n");
	//   fprintf(fp_out,"grap effa \n") ;
	//   fprintf(fp_out,"grap remp 0 \n") ;
	//   fprintf(fp_out,"grap divise cloture 4 \n") ;
	//   fprintf(fp_out,"grap sel clo 1 \n") ;
	//   fprintf(fp_out,"/g vise 1 0 0 \n") ;
	//   fprintf(fp_out,"vi \n") ;
	//   fprintf(fp_out,"grap effa 0 \n") ;
	//   fprintf(fp_out,"/g sel clo 2 \n") ;
	//   fprintf(fp_out,"/g vise 0 1 0 \n") ;
	//   fprintf(fp_out,"vi \n") ;
	//   fprintf(fp_out,"/g sel clo 3 \n") ;
	//   fprintf(fp_out,"/g vise 0 0 1 \n") ;
	//   fprintf(fp_out,"VI\n") ;
	//   fprintf(fp_out,"/g sel clo 4 \n") ;
	//   fprintf(fp_out,"grap vc visee 2 3 1\n") ;
	fprintf(fp_out, "grap remp 0 visee 2 3 1\n");
	fprintf(fp_out, "VI\n");

	// apply 3 reflections
	auto ugrid2 = reflect(ugrid);

    this->ugrid = ugrid2;	
}


void Sphere::prog1(double **coord, int noe1, int noe2, int louc, int nbe, double *xyz, double rext)
{
	int i;
	double theta, theta0, vrot[3], vabs, arg;

	vrot[0] = coord[noe1][1] * coord[noe2][2] - coord[noe1][2] * coord[noe2][1];
	vrot[1] = coord[noe1][2] * coord[noe2][0] - coord[noe1][0] * coord[noe2][2];
	vrot[2] = coord[noe1][0] * coord[noe2][1] - coord[noe1][1] * coord[noe2][0];
	vabs = sqrt(vrot[0] * vrot[0] + vrot[1] * vrot[1] + vrot[2] * vrot[2]);
	for (i = 0; i < 3; i++)
	{
		vrot[i] = vrot[i] / vabs;
	}
	arg = vabs / rext / rext;
	if (arg > 1.)
	{
		arg = 1.;
	}
	theta0 = asin(arg);
	theta = theta0 * louc / nbe;

	xyz[0] = coord[noe1][0] * cos(theta) + (vrot[1] * coord[noe1][2] - vrot[2] * coord[noe1][1]) * sin(theta);
	xyz[1] = coord[noe1][1] * cos(theta) + (vrot[2] * coord[noe1][0] - vrot[0] * coord[noe1][2]) * sin(theta);
	xyz[2] = coord[noe1][2] * cos(theta) + (vrot[0] * coord[noe1][1] - vrot[1] * coord[noe1][0]) * sin(theta);
}


vtkSmartPointer<vtkUnstructuredGrid> Sphere::reflect(vtkSmartPointer<vtkUnstructuredGrid> ugrid)
{

	
	auto rfilterX = vtkSmartPointer<vtkReflectionFilter>::New();
	rfilterX->SetInputData(ugrid);
	rfilterX->CopyInputOn();
	rfilterX->SetPlaneToXMax();

	auto rfilterY = vtkSmartPointer<vtkReflectionFilter>::New();
	rfilterY->SetInputConnection(rfilterX->GetOutputPort());
	rfilterY->CopyInputOn();
	rfilterY->SetPlaneToYMax();

	auto rfilterZ = vtkSmartPointer<vtkReflectionFilter>::New();
	rfilterZ->SetInputConnection(rfilterY->GetOutputPort());
	rfilterZ->CopyInputOn();
	rfilterZ->SetPlaneToZMax();

	auto tougrid = vtkSmartPointer<vtkExtractUnstructuredGrid>::New();
	tougrid->MergingOn();
	auto ptInserter = vtkSmartPointer<vtkIncrementalOctreePointLocator>::New();
    ptInserter->SetTolerance(0.001); // default tol is too low
	tougrid->SetLocator(ptInserter);
	tougrid->SetInputConnection(rfilterZ->GetOutputPort());
	tougrid->Update();
	vtkSmartPointer<vtkUnstructuredGrid> ugrid2 = tougrid->GetOutput();

    return ugrid2;	
}
