//   Copyright 1994-2017 Romain Boman
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
 
 /********************************
 *     Mandelbrot - Julia       *
 *                              *
 *                 Robo juin'94 *
 ********************************/


#include <graphics.h>
#include <conio.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* Variables globales */
float x1=-2, y1=1.2, x2=1.5, y2=-1.2;
float stepx=0.2, stepy=0.2, xc=0, yc=0, prm[10];
int sl=640, sh=480, nb_coul=50, inc=2, couleur='1';
int max_it=0, sav=0, time1=0, time2=0;

int main(void)
{
   /* Déclaration des fonctions */
   float mandelbrot(void);
   float julia(void);
   void modif_param(void);
   void titre(void);
   void infos(void);
   void load_param(void);
   void save_param(void);

   /* Déclaration des variables */
   float i;
   int exit=0, choix=0;
   float no=0;
   extern int max_it;

   /* Menu */
   while(exit==0)
   {
   clrscr();
   titre();
   puts("\n\t1:------> Mandelbrot");
   puts("\t2:------> Julia");
   puts("\n\t3:------> Modifier paramètres");
   puts("\t4:------> Charger paramètres");
   puts("\t5:------> Sauver paramètres");
   puts("\n\t6:------> Infos");
   puts("\t7:------> Fin");
   printf("\n  Choix \?");
   printf("\n\n\n\nNbre op: %f", no);
   printf("\nItération max: %d", max_it);
   printf("\nTemps calcul :%d",time2-time1);
      choix=getch();
      switch(choix)
      {
	 case '1':no=mandelbrot();break;
	 case '2':no=julia();break;
	 case '3':modif_param();break;
	 case '4':load_param();break;
	 case '5':save_param();break;
	 case '6':infos();break;
	 case '7':exit=1;break;
      }
   }
   clrscr();
   return 0;
}

/* Initialisation du driver VGA 640x480 et de la palette */
void init_graph(void)
{
   palettetype pal;
   int gdriver = 9, gmode = 2, i;
   extern int couleur;

   initgraph(&gdriver, &gmode, "");

   getpalette(&pal);
   switch(couleur)
      {
	 case '1':for(i=0;i<16;i++)
	    setrgbpalette(pal.colors[i],i*4,0,0);
	 break;
	 case '2':for(i=0;i<16;i++)
	    setrgbpalette(pal.colors[i],i*4,i*4,0);
	 break;
	 case '3':for(i=0;i<16;i++)
	    setrgbpalette(pal.colors[i],0,i*4,0);
	 break;
	 case '4':for(i=0;i<16;i++)
	    setrgbpalette(pal.colors[i],0,i*4,i*4);
	 break;
	 case '5':for(i=0;i<16;i++)
	    setrgbpalette(pal.colors[i],0,0,i*4);
	 break;
	 case '6':for(i=0;i<16;i++)
	    setrgbpalette(pal.colors[i],i*4,0,i*4);
	 break;
	 case '7':for(i=0;i<16;i++)
	    setrgbpalette(pal.colors[i],i*4,i*4,i*4);
	 break;
	 case '8':for(i=0;i<16;i++)
	    setrgbpalette(pal.colors[i],rand()*64,rand()*64,rand()*64);
	 break;
      }
   return;
}

/* Redéfinition des paramètres */
void param(char *descript,int par)
{  char entree[20];
   extern float prm[];
   printf(descript,prm[par]);
   gets(entree);
   if(entree!="")
      sscanf(entree,"%e",&prm[par]);
}

/* Algorithme de Mandelbrot */
float mandelbrot(void)
{
   time1=clock();
   void init_graph(void);
   void axes(void);
   void save_picture(void);
   /* Variables */
   float a1, a2, xc, yc, xn, yn, xn1, yn1;
   int n, xe, ye;
   extern float x1, y1, x2, y2;
   extern int sl, sh, nb_coul, inc, max_it, sav;
   float nb_op=0;
   max_it=0;

   /* Boucle principale */
   init_graph();
   axes();
   a1=x2-x1;
   a2=y2-y1;
   for(xe=1; xe<sl-1; xe+=inc)
   {
      for(ye=1; ye<sh-1; ye+=inc)
      {
	 xc=(a1*xe)/sl+x1;
	 yc=(a2*ye)/sh+y1;
	 nb_op+=n;
	 if((n>max_it)&&(n!=nb_coul))
	    max_it=n;
	 n=0; xn=0; yn=0;
	 while((n!=nb_coul)&&(yn*yn+xn*xn<4))
	 {
	    n+=1;
	    if(n==nb_coul-1)
	       putpixel(xe,ye,0);
	    xn1=xn*xn-yn*yn+xc;
	    yn1=2*xn*yn+yc;
	    xn=xn1;
	    yn=yn1;
	 }
	 putpixel(xe, ye, n);
      }
   }
   time2=clock();
   getch();
   if(sav==1)
      save_picture();
   closegraph();
   return(nb_op);
}

/* Algorithme de Julia */
float julia(void)
{
   void init_graph(void);
   void axes(void);
   void save_picture(void);
   /* Variables */
   float a1, a2, xn, yn, xn1, yn1;
   int n, xe, ye;
   extern float x1, y1, x2, y2, xc, yc;
   extern int sl, sh, nb_coul, inc, max_it, sav;
   float nb_op=0;
   max_it=0;

   /* Boucle principale */
   init_graph();
   axes();
   a1=x2-x1;
   a2=y2-y1;
   for(xe=1; xe<sl-1; xe+=inc)
   {
      for(ye=1; ye<sh-1; ye+=inc)
      {
	 xn=(a1*xe)/sl+x1;
	 yn=(a2*ye)/sh+y1;
	 nb_op+=n;
	 if((n>max_it)&&(n!=nb_coul))
	    max_it=n;
	 n=0;
	 while((n!=nb_coul)&&(yn*yn+xn*xn<4))
	 {
	    n+=1;
	    if(n==nb_coul-1)
	       putpixel(xe,ye,0);
	    xn1=xn*xn-yn*yn+xc;
	    yn1=2*xn*yn+yc;
	    xn=xn1;
	    yn=yn1;
	 }
	 putpixel(xe, ye, n);
      }
   }
   getch();
   if(sav==1)
      save_picture();
   closegraph();
   return(nb_op);
}

/* Trace les axes et le quadrillage */
void axes(void)
{
   /* Fonctions */
   void line_horiz(float);
   void line_vert(float);

   /* Variables */
   float i, startx, starty, endx, endy, swap;
   extern float x1, x2, y1, y2, stepx, stepy;
   extern int sl, sh;

   /* Quadrillage */
   startx=stepx*floor(x1/stepx);
   endx=stepx*(floor(x2/stepx)+1);
   starty=stepy*floor(y2/stepy);
   endy=stepy*floor(y1/stepy);
   if(startx>endx)
   {
      swap=endx;endx=startx;startx=swap;
   }
   if(starty>endy)
   {
      swap=endy;endy=starty;starty=swap;
   }
   setcolor(13);
   for(i=startx; i<=endx; i+=stepx)
      line_vert((i-x1)*sl/(x2-x1));
   for(i=starty; i<=endy; i+=stepy)
      line_horiz(sh+(i-y1)*sh/(y1-y2));

   /* Axes en (0,0) */
   line_horiz(sh-y1*sh/(y1-y2)+1);
   line_vert(-x1*sl/(x2-x1)+1);

   /* Cadre */
   rectangle(0, 0, sl-1, sh-1);

   return;
}

void line_horiz(float y)
{
   extern sl;
   line(0, y, sl, y);
   return;
}

void line_vert(float x)
{
   extern sh;
   line(x, 0, x, sh);
   return;
}

void titre(void)
{
   puts("\t\t\t       +-------------------+");
   puts("\t\t\t       | Fractal v1.0      |");
   puts("\t\t\t       |                   |");
   puts("\t\t\t       |      Robo juin'94 |");
   puts("\t\t\t       +-------------------+");
   return;
}

void var_par(int opt)
{
   extern int inc, nb_coul;
   extern float x1, y1, x2, y2, stepx, stepy, prm[], xc, yc;
   if(opt==0)
   {
   prm[0]=x1; prm[1]=y1;
   prm[2]=x2; prm[3]=y2;
   prm[4]=stepx; prm[5]=stepy;
   prm[6]=inc; prm[7]=nb_coul;
   prm[8]=xc; prm[9]=yc;
   }
   else
   {
   x1=prm[0]; y1=prm[1];
   x2=prm[2]; y2=prm[3];
   stepx=prm[4]; stepy=prm[5];
   inc=floor(prm[6]); nb_coul=floor(prm[7]);
   xc=prm[8]; yc=prm[9];
   }
   return;
}

/* Modification des paramètres */
void modif_param(void)
{
   void titre(void);
   void var_par(int);
   void axes_ortho(void);
   void couleurs(void);
   void param(char *, int);
   int i, exit=0, choix=0;
   float swap;
   extern int sav;
   char *texte[10]={"\n\nCoin sup. gauche [x1=%f] :","Coin sup. gauche [y1=%f] :",
		   "Coin inf. droit  [x2=%f] :", "Coin inf. droit  [y2=%f] :",
		   "\nQuadrillage [x=%f] :", "Quadrillage [y=%f] :",
		   "\nQualité [inc=%f] :", "Précision [coul=%f] :",
		   "\nCte Julia [xc=%f] :", "Cte Julia [yc=%f] :" };

   /* Menu */
   while(exit==0)
   {
   clrscr();
   titre();
   puts("\n\n\n\t1:------> Liste complète");
   puts("\n\t2:------> Couleurs");
   puts("\t3:------> Axes orthonormés");
   if(sav==1)
   puts("\t4:------> Sauvegarde de l'image : On");
   else
   puts("\t4:------> Sauvegarde de l'image : Off");
   puts("\n\t5:------> Menu principal");
   printf("\n\n  Choix:");
      choix=getch();
      switch(choix)
      {
	 case '1':
	    clrscr();
	    titre();
	    var_par(0);
	    for(i=0;i<10;i++)
	    param(texte[i],i);
	    var_par(1);

	    /* Teste la validité des paramètres */
	    if(x1>x2)
	    {
	       swap=x1; x1=x2; x2=swap;
	    }
	    if(y1<y2)
	    {
	    swap=y1; y1=y2; y2=swap;
	    }
	    if(stepx<0)
	       stepx=-1*stepx;
	    if(stepx>1)
	       stepx=(x2-x1)/stepx;
	    if(stepy<0)
	       stepy=-1*stepy;
	    if(stepy>1)
	       stepy=(y1-y2)/stepy;
	    if(inc<1)
	       inc=1;
	    if(nb_coul<16)
	       nb_coul=16;
	    exit=1;
	    break;
	 case '2':couleurs();exit=1;break;
	 case '3':axes_ortho();exit=1;break;
	 case '4':
	    if(sav==1)
	    sav=0;
	    else
	    sav=1;
	    break;
	 case '5':exit=1;break;
      }
   }
   return;
}

void infos(void)
{
   clrscr();
   titre();
   puts("\nMandelbrot: A chaque pt du plan complexe, le programme attribue une");
   puts("            couleur relative à la vitesse de convergence de la     ");
   puts("            suite    Zo = 0 , Zn+1 = Zn + C");
   puts("             (C étant le nbre complexe considéré)");
   puts("\nJulia: Idem mais ici, Zo = nbre complexe considéré.");
   puts("                      C  = Cte ");
   puts("\nProchaine version:");
   puts("    .Sauvegarde des images sur disque.");
   puts("    .Amélioration de la sauvegarde des paramètres.");
   puts("    .Autres algorithmes (Sierpinski, Koch, Minkowski,...)");
   puts("    .Détermination directe des param. sur une image graphique.");
   puts("");
   puts("\n\n\nProgramme écrit en C++");
   getch();
   return;
}

void load_param(void)
{
   extern float prm[];
   char *nom;
   FILE *f;
   nom="";
   clrscr();
   titre();
   puts("\n\nEntrez le nom du fichier:");
   gets(nom);
   if(strcmp(nom,"")!=0)
   {
      f=fopen(nom,"r+b");
      fread(&prm,sizeof(float),10,f);
      var_par(1);
      puts("\n\nChargement effectué, appuyez sur une touche...");
      getch();
      fclose(f);
   }
   else
   {
      puts("\n\nChargement annulé, appuyez sur une touche...");
      getch();
   }
   return;
}

void save_param(void)
{
   extern float prm[];
   char *nom;
   FILE *f;
   nom="";
   var_par(0);
   clrscr();
   titre();
   puts("\n\nEntrez le nom du fichier:");
   gets(nom);
   if(strcmp(nom,"")!=0)
   {
      f=fopen(nom,"w+b");
      fwrite(&prm,sizeof(float),10,f);
      puts("\n\nSauvegarde effectuée, appuyez sur une touche...");
      getch();
      fclose(f);
   }
   else
   {
      puts("\n\nSauvegarde annulée, appuyez sur une touche...");
      getch();
   }
   return;
}

void axes_ortho(void)
{
   extern float stepx,stepy;
   stepx=(stepx+stepy)/2;
   stepy=stepx;
   return;
}

void couleurs(void)
{
   extern int couleur;

   /* Menu */
   clrscr();
   titre();
   puts("\n\n\n\t1:------> Rouge");
   puts("\t2:------> Jaune");
   puts("\t3:------> Vert");
   puts("\t4:------> Cyan");
   puts("\t5:------> Bleu");
   puts("\t6:------> Mauve");
   puts("\t7:------> Grisâtre");
   puts("\t8:------> Aléatoire");
   printf("\n\n  Choix:");
   couleur=getch();
   return;
}

void save_picture(void)
{

}
