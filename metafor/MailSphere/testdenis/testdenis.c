// pour compiler : cc -g -lm -o test.x  test.c 
// ca cree un executable test.x
// on tape test.x et ca cree out.dat lisible dans BACON 
// pour modifier la sphere, aller voir dans paramètres ! (un peu plus bas) 

#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
void prog(double **,int,int,int,int,double *,double) ;


void main ()
{

   int nbe,nbc,noe_ini,maille_ini,mat,loi,taille,taille1,taille2,taille3,taille4,i,j,k,l,
           ****tab,*liste,**status,no,c,face,addon,n[4],s,cote,noe1,noe2,noe,louc,
           noe0,l_ini,l_fin,c_ini,c_fin,nint,maille,noe3,noe4,noe5,noe6,noe7,noe8,
           compteur,dao,couche ;

   double rint,rext,centre[3],r[3],**coord,xyz[3],alpha,ray ; 

   FILE *fp_out;

   fp_out = fopen("out.dat","w");
   
   fprintf(fp_out,".DEL.*\n") ;

// PARAMETRES

   rint = .5; // rayon interne
   rext = 1; // rayon externe
   centre[0] = 0.0; // coor x du centre
   centre[1] = 0.0; // coor y du centre
   centre[2] = 0.0; // coor z du centre
   r[0] = 0.0; // coor x du reducteur
   r[1] = 0.0; // coor y du reducteur
   r[2] = 0.0; // coor z du reducteur
   nbe = 2; // nombre d elements sur 1/6 d un méridien 
   nbc = 1; // nombre d elements sur l epaisseur
   noe_ini=0 ;
   maille_ini=0 ;
   mat=1 ;
   loi=1 ;

// FIN DES PARAMETRES


// allocation du tableau tab(couche,face,lig,col)

   taille1 = (nbc+1);
   tab = (int ****) calloc(taille1,sizeof(int ***));
   for (j = 0; j<taille1 ; j++) 
     {
       taille2=6 ;
       tab[j] = (int ***) calloc(taille2, sizeof(int **));
       for (k = 0; k<taille2 ; k++) 
	 {
	   taille3=(nbe+1) ;
	   tab[j][k] = (int **) calloc(taille3, sizeof(int *));
	   for (l = 0; l<taille3 ; l++) 
	     {
	       taille4=(nbe+1) ;
	       tab[j][k][l] = (int *) calloc(taille4, sizeof(int));
	     }
	 }
     }

// allocation du tableau coord(numint, xyz)

   taille = (6*nbe*nbe+2)*(nbc+1) ;
   coord = (double **) calloc(taille,sizeof(double *));
   for (j = 0; j<taille ; j++) {
       coord[j] = (double *) calloc(3, sizeof(double));
   }
 
//  allocation du vecteur liste(numint)=numdao

   taille = (6*nbe*nbe+2)*(nbc+1) ;
   liste = (int *) calloc(taille,sizeof(int));

// allocation du tableau status(face,l/c)

   taille = 6 ;
   status = (int **) calloc(taille,sizeof(int *));
   for (j = 0; j<taille ; j++) {
      status[j] = (int *) calloc(2, sizeof(int));
   }

// fin des allocations

// remplissage de tab

// couche 0 (exterieure)

     no=-1 ;

     for (face = 0; face < 6 ; face ++){
       for (l = 0; l < nbe+1 ; l ++){
	 for (c = 0; c < nbe+1 ; c ++){
	   if(face==1 && c == 0){     tab[0][face][l][c]=tab[0][face-1][l][nbe] ;}
	   else if(face==2 && c == 0){tab[0][face][l][c]=tab[0][face-1][l][nbe] ;}
	   else if(face==3 && c == 0){tab[0][face][l][c]=tab[0][face-1][l][nbe] ;}
	   else if(face==3 && c == nbe){tab[0][face][l][c]=tab[0][face-3][l][0] ;}
	   else if(face==4 && c == 0){tab[0][face][l][c]=tab[0][face-3][0][l] ; }
	   else if(face==4 && c == nbe){tab[0][face][l][c]=tab[0][face-1][nbe][nbe-l] ;}
	   else if(face==4 && l == 0){tab[0][face][l][c]=tab[0][face-4][0][nbe-c] ;}
	   else if(face==4 && l == nbe){tab[0][face][l][c]=tab[0][face-2][0][c] ;}
	   else if(face==5 && c == 0){tab[0][face][l][c]=tab[0][face-2][nbe][nbe-l] ;}
	   else if(face==5 && c == nbe){tab[0][face][l][c]=tab[0][face-4][nbe][l] ;}
	   else if(face==5 && l == 0){tab[0][face][l][c]=tab[0][face-5][nbe][c] ;}
	   else if(face==5 && l == nbe){tab[0][face][l][c]=tab[0][face-3][nbe][nbe-c] ;}
	   else{
	     no=no+1 ;
	     tab[0][face][l][c]=no ;
	   }
	 }
       }
     }

// On en déduit toutes les couches

     for (couche = 1; couche<=nbc; couche++){
       addon=couche*(6*nbe*nbe+2) ;
       for (face = 0; face < 6; face ++){
	 for (l = 0; l < nbe+1; l ++){
	   for (c = 0; c < nbe+1; c ++){
	       tab[couche][face][l][c]=tab[0][face][l][c]+addon ;
	   }
	 }
       }
     }

// Calcul des coordonées

// La face 0

// Les coins

     n[0]=tab[0][0][0][0] ;
     n[1]=tab[0][0][0][nbe] ;
     n[2]=tab[0][0][nbe][nbe] ;
     n[3]=tab[0][0][nbe][0] ;

     ray=rext/sqrt(3.) ;

     for (i = 0; i < 3; i++){
       for (j = 0; j < 4; j++){
	 if(((j==1 || j==2) && i==0)||((j==2 || j==3) && i==2)){s=-1 ;}
	 else{s=1 ;}

	 coord[n[j]][i]=s*ray ;
       }
     }

// Les cotes

     for(cote=0 ; cote<4 ; cote++){

       if(cote == 0){l=0 ;}
       if(cote == 1){c=nbe ;}
       if(cote == 2){l=nbe ;}
       if(cote == 3){c=0 ;}

       if(cote == 0 || cote == 3){noe1=n[0] ;}
       else if(cote == 1){noe1=n[1] ;}
       else if(cote == 2){noe1=n[3] ;}

       if(cote == 1 || cote == 2){noe2=n[2] ;}
       else if(cote == 0){noe2=n[1] ;}
       else if(cote == 3){noe2=n[3] ;}

       if(cote == 0 || cote == 2){
	 for(c = 1; c<nbe ; c ++){
	   noe=tab[0][0][l][c] ;
	   prog(coord,noe1,noe2,c,nbe,xyz,rext) ;
	   for( i = 0 ; i<3 ; i++){
	     coord[noe][i]=xyz[i] ;
	   }
	 }
       }
       else if(cote == 1 || cote == 3){
	 for(l = 1 ; l<nbe ; l ++){
	   noe=tab[0][0][l][c] ;
	   prog(coord,noe1,noe2,l,nbe,xyz,rext) ;
	   for( i = 0 ; i<3 ; i++){
	     coord[noe][i]=xyz[i] ;
	   }
	 }
       }
     }

// L intérieur

     for(l = 1 ; l < nbe ; l++){
       noe1=tab[0][0][l][0] ;
       noe2=tab[0][0][l][nbe] ;

       for(c = 1 ; c < nbe ; c++){
	 noe=tab[0][0][l][c] ;
	 prog(coord,noe1,noe2,c,nbe,xyz,rext) ;
	 for( i = 0 ; i<3 ; i++){
	   coord[noe][i]=xyz[i] ;
	 }
       }
     }

// Toutes les couches de la face 0

     for(couche = 1 ; couche < nbc+1 ; couche++){
       alpha=1+(rint/rext-1)*couche/nbc ;
       for(l = 0 ; l < nbe+1 ; l++){
	 for(c = 0 ; c < nbe+1 ; c++){
	   noe=tab[couche][0][l][c] ;
	   noe0=tab[0][0][l][c] ;
	   for( i = 0 ; i<3 ; i++){
	     coord[noe][i]=coord[noe0][i]*alpha ;
	   }
	 }
       }
     }

// generation des faces 1 a 3

     for(face = 1 ; face < 4 ; face++){
       for(couche = 0 ; couche < nbc+1 ; couche++){
	 for(l = 1 ; l < nbe ; l++){
	   for(c = 1 ; c < nbe ; c++){
	     noe=tab[couche][face][l][c] ;
	     noe0=tab[couche][face-1][l][c] ;
	     coord[noe][0]=-coord[noe0][1] ;
	     coord[noe][1]= coord[noe0][0] ;
	     coord[noe][2]= coord[noe0][2] ;
	   }
	 }
       }
     }
     
// generation de la face 4

     face=4 ; 
     for(couche = 0 ; couche < nbc+1 ; couche++){
       for(l = 1 ; l < nbe ; l++){
	 for(c = 1 ; c < nbe ; c++){
	   noe=tab[couche][face][l][c] ;
	   noe0=tab[couche][1][l][c] ;
	   coord[noe][0]= coord[noe0][2] ;
	   coord[noe][1]= coord[noe0][1] ;
	   coord[noe][2]=-coord[noe0][0] ;
	 }
       }
     }
        
// generation de la face 5

     face=5 ;
     for(couche = 0 ; couche < nbc+1 ; couche++){
       for(l = 1 ; l < nbe ; l++){
	 for(c = 1 ; c < nbe ; c++){
	   noe=tab[couche][face][l][c] ;
	   noe0=tab[couche][0][l][c] ;
	   coord[noe][0]= coord[noe0][0] ;
	   coord[noe][1]= coord[noe0][2] ;
	   coord[noe][2]=-coord[noe0][1] ;
	 }
       }
     }

// mise à jour des positions avec le centre centre[i]

     for( i = 0 ; i<3 ; i++){
       for( j = 0 ; j < ((1+nbc)*(6*nbe*nbe+2)) ; j++ ){
	 coord[j][i]=coord[j][i]+centre[i] ;
       }
     }

// fin du remplissage du tableau des coord.


// prise en compte du decoupage

     if((r[0] != 0 || r[1] != 0 || r[2] != 0) && (nbe % 2 != 0)){nbe=nbe+1 ;}

     if(r[0] > 0){
       status[0][1]=1  ;
       status[1][1]=3  ;
       status[1][2]=3  ;
       status[2][1]=2  ;
       status[4][1]=2  ;
       status[5][1]=1  ;
     }
     else if(r[0] < 0){
       status[0][1]=2  ;
       status[2][1]=1  ;
       status[3][1]=3  ;
       status[3][2]=3  ;
       status[4][1]=1  ;
       status[5][1]=2  ;
     }
     if(r[1] > 0){
       if(status[1][1] != 3){status[1][1]=1  ;}
       status[2][1]=3  ;
       status[2][2]=3  ;
       if(status[3][1] != 3){status[3][1]=2  ;}
       status[4][2]=1  ;
       status[5][2]=1  ;
     }
     else if(r[1] < 0){
       status[0][1]=3  ;
       status[0][2]=3  ;
       if(status[1][1] != 3){status[1][1]=2  ;}
       if(status[3][1] != 3){status[3][1]=1  ;}
       status[4][2]=2  ;
       status[5][2]=2  ;
     }
     if(r[2] > 0){
       if(status[0][2] != 3){status[0][2]=1  ;}
       if(status[1][2] != 3){status[1][2]=1  ;}
       if(status[2][2] != 3){status[2][2]=1  ;}
       if(status[3][2] != 3){status[3][2]=1  ;}
       status[5][1]=3  ;
       status[5][2]=3  ;
     }
     else if(r[2] < 0){
       if(status[0][2] != 3){status[0][2]=2  ;}
       if(status[1][2] != 3){status[1][2]=2  ;}
       if(status[2][2] != 3){status[2][2]=2  ;}
       if(status[3][2] != 3){status[3][2]=2  ;}
       status[4][1]=3  ;
       status[4][2]=3  ;
     }

//  impression du .NOE

     fprintf(fp_out,"\n.NOEUD\n") ;
     noe=noe_ini ;
       
     for(couche=0; couche <= nbc; couche++) {
       for(face=0; face < 6 ; face++) {
	 if(status[face][1] != 3){
	   l_ini = 0 ;
	   l_fin = nbe ;
	   c_ini = 0 ;
	   c_fin = nbe ;

	   if(status[face][1] == 1){l_fin=nbe/2 ;}
	   else if(status[face][1] == 2){l_ini=nbe/2 ;}

	   if(status[face][2] == 1){c_fin=nbe/2 ;}
	   else if(status[face][2] == 2){c_ini=nbe/2 ;}

	   for(l=l_ini; l <= l_fin ; l++) {
	     for(c=c_ini; c <= c_fin ; c++) {
	       nint=tab[couche][face][l][c] ;
	       if(liste[nint] == 0){
		 noe=noe+1 ;
		 fprintf(fp_out,"I %8d X %15.8E Y %15.8E Z %15.8E\n",noe,coord[nint][0],coord[nint][1],coord[nint][2]) ;
		 liste[nint] = noe ;
	       }
	     }
	   }
	 }
       }
     }

// impression du .MAI

     fprintf(fp_out,"\n.MAI\n") ;
     maille=maille_ini ;
       
     for(couche=1; couche <= nbc; couche++) {
       for(face=0; face < 6 ; face++) {
	 if(status[face][1] != 3){
	   l_ini = 0 ;
	   l_fin = nbe-1 ;
	   c_ini = 0 ;
	   c_fin = nbe-1 ;

	   if(status[face][1] == 1){l_fin=nbe/2 ;}
	   else if(status[face][1] == 2){l_ini=nbe/2 ;}

	   if(status[face][2] == 1){c_fin=nbe/2 ;}
	   else if(status[face][2] == 2){c_ini=nbe/2 ;}

	   for(l=l_ini; l <= l_fin ; l++) {
	     for(c=c_ini; c <= c_fin ; c++) {
	       maille=maille+1 ;
	       noe1=tab[couche-1][face][l][c] ;
	       noe2=tab[couche-1][face][l][c+1] ;
	       noe3=tab[couche-1][face][l+1][c+1] ;
	       noe4=tab[couche-1][face][l+1][c] ;
	       noe5=tab[couche][face][l][c] ;
	       noe6=tab[couche][face][l][c+1] ;
	       noe7=tab[couche][face][l+1][c+1] ;
	       noe8=tab[couche][face][l+1][c] ;
	       fprintf(fp_out,"I %8d N %d %d %d %d 0 %d %d %d %d\n",maille,
		       liste[noe1],liste[noe2],liste[noe3],liste[noe4],
		       liste[noe5],liste[noe6],liste[noe7],liste[noe8]) ;
	       }
	     }
	   }
	 }
       }

//  impression du .MCO

     fprintf(fp_out,"\n .MCO \n") ;
       
//   couche externe

     couche=0 ;
     compteur=0 ;

     for(face=0; face < 6 ; face++) {
       if(status[face][1] != 3){

	 if(compteur > 0){fprintf(fp_out,"I %8d \n",-4) ;}
	 compteur=compteur+1 ;

	 l_ini = 0 ;
	 l_fin = nbe ;
	 c_ini = 0 ;
	 c_fin = nbe ;

	 if(status[face][1] == 1){l_fin=nbe/2 ;}
	 else if(status[face][1] == 2){l_ini=nbe/2 ;}

	 if(status[face][2] == 1){c_fin=nbe/2 ;}
	 else if(status[face][2] == 2){c_ini=nbe/2 ;}

	 for(l=l_ini; l <= l_fin ; l++) {
	   for(c=c_ini; c <= c_fin ; c++) {
	     noe=tab[couche][face][l][c] ;
	     dao=liste[noe] ;
	     fprintf(fp_out,"I %8d MAT %d LOI %d \n",dao,mat,loi) ;
	   }
	   if(l != l_fin){fprintf(fp_out,"I %8d \n",-3) ;}
	 }
       }
     }

//   couche interne

     couche=nbc ;
     compteur=0 ;

     for(face=0; face < 6 ; face++) {
       if(status[face][1] != 3){

	 if(compteur > 0){fprintf(fp_out,"I %8d \n",-4) ;}
	 compteur=compteur+1 ; 

	 l_ini = 0 ;
	 l_fin = nbe ;
	 c_ini = 0 ;
	 c_fin = nbe ;

	 if(status[face][1] == 1){l_fin=nbe/2 ;}
	 else if(status[face][1] == 2){l_ini=nbe/2 ;}

	 if(status[face][2] == 1){c_fin=nbe/2 ;}
	 else if(status[face][2] == 2){c_ini=nbe/2 ;}

	 for(l=l_ini; l <= l_fin ; l++) {
	   for(c=c_ini; c <= c_fin ; c++) {
	     noe=tab[couche][face][(l_fin+l_ini)-l][c] ;
	     dao=liste[noe] ;
	     fprintf(fp_out,"I %8d MAT %d LOI %d \n",dao,mat,loi) ;
	   }
	   if(l != l_fin){fprintf(fp_out,"I %8d \n",-3) ;}
	 }
       }
     }
}

void prog(double **coord,int noe1,int noe2,int louc,int nbe,double *xyz,double rext)
  {
    int i ;
    double theta,theta0,vrot[3],vabs,arg ; 

    vrot[0]=coord[noe1][1]*coord[noe2][2]-coord[noe1][2]*coord[noe2][1] ;
    vrot[1]=coord[noe1][2]*coord[noe2][0]-coord[noe1][0]*coord[noe2][2] ;
    vrot[2]=coord[noe1][0]*coord[noe2][1]-coord[noe1][1]*coord[noe2][0] ;
    vabs=sqrt(vrot[0]*vrot[0]+vrot[1]*vrot[1]+vrot[2]*vrot[2]) ;
    for( i = 0 ; i<3 ; i++){
      vrot[i]=vrot[i]/vabs ;
    }
    arg=vabs/rext/rext ;
    theta0=asin(arg) ;
    theta=theta0*louc/nbe ;
    xyz[0]=coord[noe1][0]*cos(theta)+(vrot[1]*coord[noe1][2]-vrot[2]*coord[noe1][1])*sin(theta) ;
    xyz[1]=coord[noe1][1]*cos(theta)+(vrot[2]*coord[noe1][0]-vrot[0]*coord[noe1][2])*sin(theta) ;
    xyz[2]=coord[noe1][2]*cos(theta)+(vrot[0]*coord[noe1][1]-vrot[1]*coord[noe1][0])*sin(theta) ;
  }
