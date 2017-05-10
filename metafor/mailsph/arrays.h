#ifndef ARRAYS_H
#define ARRAYS_H


template <typename T>
void array4D_alloc(T ****&tab, int taille1, int taille2, int taille3, int taille4)
{
	tab = (T ****)calloc(taille1, sizeof(T ***));
	for (int j = 0; j < taille1; j++)
	{
		tab[j] = (T ***)calloc(taille2, sizeof(T **));
		for (int k = 0; k < taille2; k++)
		{
			tab[j][k] = (T **)calloc(taille3, sizeof(T *));
			for (int l = 0; l < taille3; l++)
				tab[j][k][l] = (T *)calloc(taille4, sizeof(T));
		}
	}
}

template <typename T>
void array4D_free(T ****&tab, int taille1, int taille2, int taille3)
{
	for (int j = 0; j < taille1; j++)
	{
		for (int k = 0; k < taille2; k++)
		{
			for (int l = 0; l < taille3; l++)
				free(tab[j][k][l]);
			free(tab[j][k]);
		}
		free(tab[j]);
	}
	free(tab);
}

template <typename T>
void array3D_alloc(T ***&tab, int taille1, int taille2, int taille3)
{
	tab = (T ***)calloc(taille1, sizeof(T **));
	for (int j = 0; j < taille1; j++)
	{
		tab[j] = (T **)calloc(taille2, sizeof(T *));
		for (int k = 0; k < taille2; k++)
			tab[j][k] = (T *)calloc(taille3, sizeof(T));
	}
}

template <typename T>
void array3D_free(T ***&tab, int taille1, int taille2)
{
	for (int j = 0; j < taille1; j++)
	{
		for (int k = 0; k < taille2; k++)
			free(tab[j][k]);
		free(tab[j]);
	}
	free(tab);
}

template <typename T>
void array2D_alloc(T **&tab, int taille1, int taille2)
{
	tab = (T **)calloc(taille1, sizeof(T *));
	for (int j = 0; j < taille1; j++)
		tab[j] = (T *)calloc(taille2, sizeof(T));
}

template <typename T>
void array2D_free(T **&tab, int taille1)
{
	for (int j = 0; j < taille1; j++)
		free(tab[j]);
	free(tab);
}

template <typename T>
void array1D_alloc(T *&tab, int taille1)
{
	tab = (T *)calloc(taille1, sizeof(T));
}

template <typename T>
void array1D_free(T *&tab)
{
	free(tab);
}


#endif