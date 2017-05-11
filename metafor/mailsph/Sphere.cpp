#include "Sphere.h"
#include "arrays.h"
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
	int nbe, nbc, noe_ini, maille_ini, mat1, loi1, mat2, loi2, taille, taille2,
		i, j, l, ****tab = nullptr, *liste = nullptr, **status = nullptr, no, c, face, addon,
		n[4], s, cote, noe1, noe2, noe, noe0, l_ini, l_fin, c_ini, c_fin, nint,
		maille, noe3, noe4, noe5, noe6, noe7, noe8, couche,
		type1, type2, sphere_creuse, ***cube = nullptr, ep, ep_fin, ep_ini, nn[4], dim_status, nosph;

	double rint, rext, centre[3], r[3], **coord, xyz[3], ray;

	// PARAMETRES

	sphere_creuse = 1; // 1 si creuse, 0 si pleine
	rint = 12. * 5;	// rayon interne si creuse, demi-diagonale du cube central si pleine
	rext = 20. * 5;	// rayon externe
	centre[0] = 0.0;   // coor x du centre
	centre[1] = 200.0; // coor y du centre
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
		type2 = -1;

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
		mat1 = -mat1;
	if (type2 == 1)
		mat2 = -mat2;


	// allocation du tableau tab(couche,face,lig,col)
	array4D_alloc(tab, nbc + 1, 6, nbe + 1, nbe + 1);

	// allocation du tableau cube(couche,lig,col)
	if (sphere_creuse == 0)
		array3D_alloc(cube, nbe + 1, nbe + 1, nbe + 1);

	// allocation du tableau coord(numint, xyz)
	taille = (6 * nbe * nbe + 2) * (nbc + 1);
	if (sphere_creuse == 0)
		taille = taille + (nbe - 1) * (nbe - 1) * (nbe - 1);
	array2D_alloc(coord, taille, 3);

	//  allocation du vecteur liste(numint)=numdao
	taille = (6 * nbe * nbe + 2) * (nbc + 1);
	if (sphere_creuse == 0)
		taille = taille + (nbe - 1) * (nbe - 1) * (nbe - 1);
	array1D_alloc(liste, taille);

	// allocation du tableau status(face,l/c)

	dim_status = 6;
	if (sphere_creuse == 0)
		dim_status = dim_status + 1;

	status = (int **)calloc(dim_status, sizeof(int *));
	for (j = 0; j < dim_status; j++)
	{
		taille2 = 2;
		if (j == 6)
			taille2 = taille2 + 1;
		status[j] = (int *)calloc(taille2, sizeof(int));
	}

	// fin des allocations ---------------------------------------------------------

	// remplissage de tab

	// couche 0 (exterieure)

	no = -1;

	for (face = 0; face < 6; face++)
		for (l = 0; l < nbe + 1; l++)
			for (c = 0; c < nbe + 1; c++)
			{
				if (face == 1 && c == 0)
					tab[0][face][l][c] = tab[0][face - 1][l][nbe];
				else if (face == 2 && c == 0)
					tab[0][face][l][c] = tab[0][face - 1][l][nbe];
				else if (face == 3 && c == 0)
					tab[0][face][l][c] = tab[0][face - 1][l][nbe];
				else if (face == 3 && c == nbe)
					tab[0][face][l][c] = tab[0][face - 3][l][0];
				else if (face == 4 && c == 0)
					tab[0][face][l][c] = tab[0][face - 3][0][l];
				else if (face == 4 && c == nbe)
					tab[0][face][l][c] = tab[0][face - 1][0][nbe - l];
				else if (face == 4 && l == 0)
					tab[0][face][l][c] = tab[0][face - 4][0][nbe - c];
				else if (face == 4 && l == nbe)
					tab[0][face][l][c] = tab[0][face - 2][0][c];
				else if (face == 5 && c == 0)
					tab[0][face][l][c] = tab[0][face - 2][nbe][nbe - l];
				else if (face == 5 && c == nbe)
					tab[0][face][l][c] = tab[0][face - 4][nbe][l];
				else if (face == 5 && l == 0)
					tab[0][face][l][c] = tab[0][face - 5][nbe][c];
				else if (face == 5 && l == nbe)
					tab[0][face][l][c] = tab[0][face - 3][nbe][nbe - c];
				else
				{
					no = no + 1;
					tab[0][face][l][c] = no;
				}
			}

	// On en d�duit toutes les couches

	for (couche = 1; couche <= nbc; couche++)
	{
		addon = couche * (6 * nbe * nbe + 2);
		for (face = 0; face < 6; face++)
			for (l = 0; l < nbe + 1; l++)
				for (c = 0; c < nbe + 1; c++)
					tab[couche][face][l][c] = tab[0][face][l][c] + addon;
	}

	// remplissage de cube

	if (sphere_creuse == 0)
	{
		no = no + nbc * (6 * nbe * nbe + 2);
		for (ep = 0; ep < nbe + 1; ep++)
			for (l = 0; l < nbe + 1; l++)	
				for (c = 0; c < nbe + 1; c++)
				{
					if (ep == 0)
						cube[ep][l][c] = tab[nbc][0][l][c];
					else if (ep == nbe)
						cube[ep][l][c] = tab[nbc][2][l][nbe - c];
					else if (l == 0)
						cube[ep][l][c] = tab[nbc][4][ep][nbe - c];
					else if (l == nbe)
						cube[ep][l][c] = tab[nbc][5][ep][c];
					else if (c == 0)
						cube[ep][l][c] = tab[nbc][3][l][nbe - ep];
					else if (c == nbe)
						cube[ep][l][c] = tab[nbc][1][l][ep];
					else
					{
						no = no + 1;
						cube[ep][l][c] = no;
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
		for (j = 0; j < 4; j++)
		{
			if (((j == 1 || j == 2) && i == 0) || ((j == 2 || j == 3) && i == 2))
				s = -1;
			else
				s = 1;
			coord[n[j]][i] = s * ray;
		}

	// couche interieure

	nn[0] = tab[nbc][0][0][0];
	nn[1] = tab[nbc][0][0][nbe];
	nn[2] = tab[nbc][0][nbe][nbe];
	nn[3] = tab[nbc][0][nbe][0];
	ray = rint / rext;
	for (i = 0; i < 3; i++)
		for (j = 0; j < 4; j++)
			coord[nn[j]][i] = coord[n[j]][i] * ray;


	// Les cotes

	// couche exterieure

	for (cote = 0; cote < 4; cote++)
	{
		if (cote == 0)
			l = 0;
		if (cote == 1)
			c = nbe;
		if (cote == 2)
			l = nbe;
		if (cote == 3)
			c = 0;

		if (cote == 0 || cote == 3)
			noe1 = n[0];
		else if (cote == 1)
			noe1 = n[1];
		else if (cote == 2)
			noe1 = n[3];

		if (cote == 1 || cote == 2)
			noe2 = n[2];
		else if (cote == 0)
			noe2 = n[1];
		else if (cote == 3)
			noe2 = n[3];


		if (cote == 0 || cote == 2)
		{
			for (c = 1; c < nbe; c++)
			{
				noe = tab[0][0][l][c];
				prog1(coord, noe1, noe2, c, nbe, xyz, rext);
				for (i = 0; i < 3; i++)
					coord[noe][i] = xyz[i];
			}
		}
		else if (cote == 1 || cote == 3)
		{
			for (l = 1; l < nbe; l++)
			{
				noe = tab[0][0][l][c];
				prog1(coord, noe1, noe2, l, nbe, xyz, rext);
				for (i = 0; i < 3; i++)
					coord[noe][i] = xyz[i];
			}
		}
	}

	// Couche interieure

	if (sphere_creuse == 0)
	{
		for (cote = 0; cote < 4; cote++)
		{
			if (cote == 0)
				l = 0;
			if (cote == 1)
				c = nbe;
			if (cote == 2)
				l = nbe;
			if (cote == 3)
				c = 0;

			if (cote == 0 || cote == 3)
				noe1 = nn[0];
			else if (cote == 1)
				noe1 = nn[1];
			else if (cote == 2)
				noe1 = nn[3];

			if (cote == 1 || cote == 2)
				noe2 = nn[2];
			else if (cote == 0)
				noe2 = nn[1];
			else if (cote == 3)
				noe2 = nn[3];


			if (cote == 0 || cote == 2)
			{
				for (c = 1; c < nbe; c++)
				{
					noe = tab[nbc][0][l][c];
					for (i = 0; i < 3; i++)
						coord[noe][i] = coord[noe1][i] + (c * 1.) / (nbe * 1.) * (coord[noe2][i] - coord[noe1][i]);
				}
			}
			else if (cote == 1 || cote == 3)
			{
				for (l = 1; l < nbe; l++)
				{
					noe = tab[nbc][0][l][c];
					for (i = 0; i < 3; i++)
						coord[noe][i] = coord[noe1][i] + (l * 1.) / (nbe * 1.) * (coord[noe2][i] - coord[noe1][i]);
				}
			}
		}
	}

	// L interieur

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
				coord[noe][i] = xyz[i];
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
					coord[noe][i] = coord[noe1][i] + (c * 1.) / (nbe * 1.) * (coord[noe2][i] - coord[noe1][i]);		
			}
		}
	}
	else if (sphere_creuse == 1)
	{
		for (l = 0; l < nbe + 1; l++)
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

	// Toutes les couches de la face 0

	if (nbc > 1)
		for (couche = 1; couche < nbc; couche++)
			for (l = 0; l < nbe + 1; l++)
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


	// generation des faces 1 a 3

	for (face = 1; face < 4; face++)
	{
		c_ini = 1;
		c_fin = nbe + 1;
		if (face == 3)
			c_fin = nbe;
		for (couche = 0; couche < nbc + 1; couche++)
			for (l = 0; l < nbe + 1; l++)
				for (c = c_ini; c < c_fin; c++)
				{
					noe = tab[couche][face][l][c];
					noe0 = tab[couche][face - 1][l][c];
					coord[noe][0] = -coord[noe0][1];
					coord[noe][1] = coord[noe0][0];
					coord[noe][2] = coord[noe0][2];
				}
	}

	// generation de la face 4

	face = 4;
	for (couche = 0; couche < nbc + 1; couche++)
		for (l = 1; l < nbe; l++)
			for (c = 1; c < nbe; c++)
			{
				noe = tab[couche][face][l][c];
				noe0 = tab[couche][2][l][c];
				coord[noe][0] = coord[noe0][0];
				coord[noe][1] = coord[noe0][2];
				coord[noe][2] = -coord[noe0][1];
			}

	// generation de la face 5

	face = 5;
	for (couche = 0; couche < nbc + 1; couche++)
		for (l = 1; l < nbe; l++)
			for (c = 1; c < nbe; c++)
			{
				noe = tab[couche][face][l][c];
				noe0 = tab[couche][0][l][c];
				coord[noe][0] = coord[noe0][0];
				coord[noe][1] = coord[noe0][2];
				coord[noe][2] = -coord[noe0][1];
			}

	// generation du cube central

	if (sphere_creuse == 0)
	{
		if (nbe > 1)
			for (ep = 1; ep < nbe; ep++)
				for (l = 1; l < nbe; l++)
					for (c = 1; c < nbe; c++)
					{
						noe = cube[ep][l][c];
						ray = rint / sqrt(3.);
						coord[noe][0] = ray * (1 - ((2 * c) * 1.) / (nbe * 1.));
						coord[noe][1] = ray * (1 - ((2 * ep) * 1.) / (nbe * 1.));
						coord[noe][2] = ray * (1 - ((2 * l) * 1.) / (nbe * 1.));
					}
	}

	// mise � jour des positions avec le centre centre[i]

	for (i = 0; i < 3; i++)
	{
		taille = (1 + nbc) * (6 * nbe * nbe + 2);
		if (sphere_creuse == 0)
			taille = taille + (nbe - 1) * (nbe - 1) * (nbe - 1);
		for (j = 0; j < taille; j++)
			coord[j][i] = coord[j][i] + centre[i];
	}

	// fin du remplissage du tableau des coord.

	// prise en compte du decoupage

	for (i = 0; i < dim_status; i++)
	{
		for (j = 0; j < 2; j++)
			status[i][j] = 0;
		if (i == 6)
			status[i][2] = 0;
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
				status[6][0] = 1;
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
				status[6][0] = 2;
		}
		if (r[1] > 0.)
		{
			if (status[1][0] != 3)
				status[1][0] = 1;
			status[2][0] = 3;
			status[2][1] = 3;
			if (status[3][0] != 3)
				status[3][0] = 2;
			status[4][1] = 1;
			status[5][1] = 1;
			if (sphere_creuse == 0)
				status[6][2] = 1;
		}
		else if (r[1] < 0.)
		{
			status[0][0] = 3;
			status[0][1] = 3;
			if (status[1][0] != 3)
				status[1][0] = 2;
			if (status[3][0] != 3)
				status[3][0] = 1;
			status[4][1] = 2;
			status[5][1] = 2;
			if (sphere_creuse == 0)
				status[6][2] = 2;
		}
		if (r[2] > 0.)
		{
			if (status[0][1] != 3)
				status[0][1] = 1;
			if (status[1][1] != 3)
				status[1][1] = 1;
			if (status[2][1] != 3)
				status[2][1] = 1;
			if (status[3][1] != 3)
				status[3][1] = 1;
			status[5][0] = 3;
			status[5][1] = 3;
			if (sphere_creuse == 0)
				status[6][1] = 1;
		}
		else if (r[2] < 0.)
		{
			if (status[0][1] != 3)
				status[0][1] = 2;
			if (status[1][1] != 3)
				status[1][1] = 2;
			if (status[2][1] != 3)
				status[2][1] = 2;
			if (status[3][1] != 3)
				status[3][1] = 2;
			status[4][0] = 3;
			status[4][1] = 3;
			if (sphere_creuse == 0)
				status[6][1] = 2;
		}
	}

	// nodes

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
					c_fin = nbe / 2;			
				else if (status[face][0] == 2)	
					c_ini = nbe / 2;	

				if (status[face][1] == 1)
					l_fin = nbe / 2;
				else if (status[face][1] == 2)
					l_ini = nbe / 2;
	
				for (l = l_ini; l <= l_fin; l++)
					for (c = c_ini; c <= c_fin; c++)
					{
						nint = tab[couche][face][l][c];
						if (liste[nint] == 0)
						{
							noe = noe + 1;
							points->InsertPoint(noe-1, coord[nint][0], coord[nint][1], coord[nint][2]);
							liste[nint] = noe;
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
			c_fin = nbe / 2;
		else if (status[face][0] == 2)
			c_ini = nbe / 2;

		if (status[face][1] == 1)
			l_fin = nbe / 2;
		else if (status[face][1] == 2)
			l_ini = nbe / 2;

		if (status[face][2] == 1)
			ep_fin = nbe / 2;
		else if (status[face][2] == 2)
			ep_ini = nbe / 2;
	
		for (ep = ep_ini; ep <= ep_fin; ep++)
			for (l = l_ini; l <= l_fin; l++)
				for (c = c_ini; c <= c_fin; c++)
				{
					nint = cube[ep][l][c];
					if (liste[nint] == 0)
					{
						noe = noe + 1;
						points->InsertPoint(noe-1, coord[nint][0], coord[nint][1], coord[nint][2]);
						liste[nint] = noe;
					}
				}
	}

	// cells

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
					c_fin = nbe / 2 - 1;
				else if (status[face][0] == 2)
					c_ini = nbe / 2;

				if (status[face][1] == 1)
					l_fin = nbe / 2 - 1;
				else if (status[face][1] == 2)
					l_ini = nbe / 2;

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
						insertvtkcell(ugrid, liste[noe1]-1, liste[noe2] - 1, liste[noe3] - 1, liste[noe4] - 1,
									  liste[noe5] - 1, liste[noe6] - 1, liste[noe7] - 1, liste[noe8] - 1);
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
			c_fin = nbe / 2 - 1;
		else if (status[face][0] == 2)
			c_ini = nbe / 2;

		if (status[face][1] == 1)
			l_fin = nbe / 2 - 1;
		else if (status[face][1] == 2)
			l_ini = nbe / 2;

		if (status[face][2] == 1)
			ep_fin = nbe / 2 - 1;
		else if (status[face][2] == 2)
			ep_ini = nbe / 2;

		for (ep = ep_ini; ep <= ep_fin; ep++)
			for (l = l_ini; l <= l_fin; l++)
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
					insertvtkcell(ugrid, liste[noe1] - 1, liste[noe2] - 1, liste[noe3] - 1, liste[noe4] - 1,
								  liste[noe5] - 1, liste[noe6] - 1, liste[noe7] - 1, liste[noe8] - 1);
				}
	}

	// apply 3 reflections
	auto ugrid2 = reflect(ugrid);

	this->ugrid = ugrid2;

	// free memory ----------------------------------------------------

	// allocation du tableau tab(couche,face,lig,col)
	array4D_free(tab, nbc + 1, 6, nbe + 1);

	// allocation du tableau cube(couche,lig,col)
	if (sphere_creuse == 0)
		array3D_free(cube, nbe + 1, nbe + 1);

	// allocation du tableau coord(numint, xyz)
	taille = (6 * nbe * nbe + 2) * (nbc + 1);
	if (sphere_creuse == 0)
		taille = taille + (nbe - 1) * (nbe - 1) * (nbe - 1);
	array2D_free(coord, taille);

	//  allocation du vecteur liste(numint)=numdao
	array1D_free(liste);

	// allocation du tableau status(face,l/c)
	dim_status = 6;
	if (sphere_creuse == 0)
		dim_status = dim_status + 1;

	for (j = 0; j < dim_status; j++)
	{
		taille2 = 2;
		if (j == 6)
			taille2 = taille2 + 1;
		free(status[j]);
	}
	free(status);
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
