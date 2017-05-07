// compiler avec "g++ -o etage.exe -Igmm etage.cpp"
// exécuter avec "./etage.exe"
#include <iostream>
#include <gmm.h>
#include <math.h>
#include <fstream>

int main()
{   
	int m=101;//définit le nombre de noeuds (impair!)
	int k=0;//définit le pas de temps
	double kappa=170;
	double rho=2300;
	double cv=711;
	double Q=1e8;
	double f=1e5;
	double E=1.58e11;
	double alpha=2.5e-3;
	double T0=273;
	double L=45e-6;
	double deltat=(1./1.)*1e-7;
	double gamma=0.5;
	double beta=0.25;
	
	gmm::row_matrix<gmm::wsvector<double> > A(m, m);
	gmm::row_matrix<gmm::wsvector<double> > B(m, m);
	gmm::row_matrix<gmm::wsvector<double> > C(m, m);
	//schéma de Newmark s'écrit pour la température: Aprime*Tnplus1=N*Tn+Nmoins1*Tnmoins1+fct(Fnplus1, Fn, Fnmoins1)
	//schéma de Newmark s'écrit pour le déplacement: Aprimepouru*Unplus1=Npouru*Un+Nmoins1pouru*Unmoins1+fct(Tnplus1, Tn, Tnmoins1)
	gmm::row_matrix<gmm::wsvector<double> > Aprime(m, m);
	gmm::row_matrix<gmm::wsvector<double> > Aprimepouru(m, m);
	gmm::row_matrix<gmm::wsvector<double> > N(m, m);
	gmm::row_matrix<gmm::wsvector<double> > Nmoins1(m, m);
	gmm::row_matrix<gmm::wsvector<double> > Npouru(m, m);
	gmm::row_matrix<gmm::wsvector<double> > Nmoins1pouru(m, m);	
	std::vector<double> Fnplus1(m), Fn(m), Fnmoins1(m), Unplus1(m), Un(m), Unmoins1(m), Tnplus1(m), Tn(m), Tnmoins1(m), bprime(m), bprimepouru(m);
	gmm::clear(A);
	gmm::clear(B);
	gmm::clear(C);
	gmm::clear(Aprime);
	gmm::clear(Aprimepouru);
	gmm::clear(N);
	gmm::clear(Nmoins1);
	gmm::clear(Npouru);
	gmm::clear(Nmoins1pouru);
	
	// Initialisation des 3 matrices A, B, C, et des 7 vecteurs (on initialise pas Unplus1 et Tnplus1):
	
	for(int i=0;i<m;i++){
		if(i==0){
			A(i,i)=2*L/(6*(m-1));
			A(i,i+1)=1*L/(6*(m-1));
			B(i,i)=-0.5;
			B(i,i+1)=-0.5;
			C(i,i)=1*(m-1)/L;
			C(i,i+1)=-1*(m-1)/L;
			Fnplus1[i]=0;
			Fn[i]=0;
			Fnmoins1[i]=0;
			Un[i]=0;
			Unmoins1[i]=0;
			Tn[i]=T0;
			Tnmoins1[i]=T0;
		}
		else if(i==m-1){
			A(i,i)=2*L/(6*(m-1));
			A(i,i-1)=1*L/(6*(m-1));
			B(i,i)=0.5;
			B(i,i-1)=0.5;
			C(i,i)=1*(m-1)/L;
			C(i,i-1)=-1*(m-1)/L;
			Fnplus1[i]=0;
			Fn[i]=0;
			Fnmoins1[i]=0;
			Un[i]=0;
			Unmoins1[i]=0;
			Tn[i]=T0;
			Tnmoins1[i]=T0;
		}
		else{
			A(i,i)=4*L/(6*(m-1));
			A(i,i-1)=1*L/(6*(m-1));
			A(i,i+1)=1*L/(6*(m-1));
			B(i,i-1)=0.5;
			B(i,i+1)=-0.5;
			C(i,i)=2*(m-1)/L;
			C(i,i-1)=-1*(m-1)/L;
			C(i,i+1)=-1*(m-1)/L;
			Fnmoins1[i]=0;
			Un[i]=0;
			Unmoins1[i]=0;
			Tn[i]=T0;
			Tnmoins1[i]=T0;
			if(i==(m-1)/2){
				Fn[i]=Q*(1+cos(2*M_PI*f*k*deltat-M_PI));
				Fnplus1[i]=Q*(1+cos(2*M_PI*f*(k+1)*deltat-M_PI));
			}
			else{
				
				Fn[i]=0;
				Fnplus1[i]=0;
			}			
		}
	}
	
	//Création des matrices Aprime, Aprimepouru et des vecteurs bprime et bprimepouru selon le schéma de Newmark.
	//On résoudra ensuite : Aprime*Tnplus1=bprime d'une part et Aprimepouru*Unplus1=bprimepouru d'autre part.
	//Conditions aux limites
	Aprime(0,0)=1;
	Aprime(m-1,m-1)=1;
	Aprimepouru(0,0)=1;
	Aprimepouru(m-1,m-1)=1;
	bprime[0]=T0;
	bprime[m-1]=T0;
	bprimepouru[0]=0;
	bprimepouru[m-1]=0;
	for(int i=1;i<m-1;i++){
		for(int j=i-1;j<=i+1;j++){
			Aprime(i,j)=gamma*deltat*rho*cv*A(i,j)+beta*deltat*deltat*kappa*C(i,j);//ne change plus
			Aprimepouru(i,j)=rho*A(i,j)+beta*deltat*deltat*E*C(i,j);//ne change plus
			N(i,j)=-(1-2*gamma)*deltat*rho*cv*A(i,j)-(0.5+gamma-2*beta)*deltat*deltat*kappa*C(i,j);//intermédiaire pour calcul de bprime
			Nmoins1(i,j)=-(gamma-1)*deltat*rho*cv*A(i,j)-(0.5-gamma+beta)*deltat*deltat*kappa*C(i,j);//intermédiaire pour calcul de bprime
			Npouru(i,j)=2*rho*A(i,j)-(0.5+gamma-2*beta)*deltat*deltat*E*C(i,j);//intermédiaire pour calcul de bprimepouru
			Nmoins1pouru(i,j)=(-rho*A(i,j)-(0.5-gamma+beta)*deltat*deltat*E*C(i,j));//intermédiaire pour calcul de bprimepouru
		}	
	}

	//Ecriture des conditions initiales dans le fichier
	std::ofstream myfile("Temp.txt");
	for(int i=0;i<m;i++){
		myfile << Tn[i] << ";";
	}
	myfile << "\n";
	for(int i=0;i<m;i++){
		myfile << Un[i] << ";";
	}
	myfile << "\n";
	
	//Calcul des deux matrices de préconditionnement et définition du critère d'erreur
	gmm::ilutp_precond<gmm::row_matrix<gmm::wsvector<double> > > P(Aprime, m, 0.);
	gmm::iteration iter(1.e-6);
	gmm::ilutp_precond<gmm::row_matrix<gmm::wsvector<double> > > Ppouru(Aprimepouru, m, 0.);
	gmm::iteration iterpouru(1.e-6);
	
	///////////////////////////////
	//Boucle sur les pas de temps//
	///////////////////////////////
	
	for(k=1;k<1000;k++){
	
		//////////////////////////////////////////////
		//Résolution de l'équation de la température//
		//////////////////////////////////////////////
		//Calcul de bprime
		for(int i=1;i<m-1;i++){
				bprime[i]=N(i,i)*Tn[i]+Nmoins1(i,i)*Tnmoins1[i]+deltat*deltat*(beta*Fnplus1[i]+(0.5+gamma-2*beta)*Fn[i]+(0.5-gamma+beta)*Fnmoins1[i]);
				bprime[i]=bprime[i]+N(i,i-1)*Tn[i-1]+Nmoins1(i,i-1)*Tnmoins1[i-1];
				bprime[i]=bprime[i]+N(i,i+1)*Tn[i+1]+Nmoins1(i,i+1)*Tnmoins1[i+1];
		}
        //Résolution de l'équation
		gmm::gmres(Aprime, Tnplus1, bprime, P, 100, iter);
		//Ecriture de la solution dans un fichier
		for(int i=0;i<m;i++){
			myfile << Tnplus1[i] << ";";
		}
		myfile << "\n";
		/////////////////////////////////////////////
		//Résolution de l'équation des déplacements//
		/////////////////////////////////////////////
		//Calcul de bprimepouru
		for(int i=1;i<m-1;i++){
				bprimepouru[i]=Npouru(i,i)*Un[i]+Nmoins1pouru(i,i)*Unmoins1[i]+deltat*deltat*(beta*alpha*E*B(i,i)*Tnplus1[i]+(0.5+gamma-2*beta)*alpha*E*B(i,i)*Tn[i]+(0.5-gamma+beta)*alpha*E*B(i,i)*Tnmoins1[i]);
				bprimepouru[i]=bprimepouru[i]+Npouru(i,i-1)*Un[i-1]+Nmoins1pouru(i,i-1)*Unmoins1[i-1]+deltat*deltat*(beta*alpha*E*B(i,i-1)*Tnplus1[i-1]+(0.5+gamma-2*beta)*alpha*E*B(i,i-1)*Tn[i-1]+(0.5-gamma+beta)*alpha*E*B(i,i-1)*Tnmoins1[i-1]);
				bprimepouru[i]=bprimepouru[i]+Npouru(i,i+1)*Un[i+1]+Nmoins1pouru(i,i+1)*Unmoins1[i+1]+deltat*deltat*(beta*alpha*E*B(i,i+1)*Tnplus1[i+1]+(0.5+gamma-2*beta)*alpha*E*B(i,i+1)*Tn[i+1]+(0.5-gamma+beta)*alpha*E*B(i,i+1)*Tnmoins1[i+1]);
		}
        //Résolution de l'équation
		gmm::gmres(Aprimepouru, Unplus1, bprimepouru, Ppouru, 100, iterpouru);
		//Ecriture de la solution dans un fichier
		for(int i=0;i<m;i++){
			myfile << Unplus1[i] << ";";
		}
		myfile << "\n";

		//Actualisation des vecteurs F, des 2 vecteurs T (Tnmoins1 et Tn) et des 2 vecteurs U (Unmoins1 et Un) pour le pas de temps suivant.
		Fnmoins1[(m-1)/2]=Fn[(m-1)/2];
		Fn[(m-1)/2]=Fnplus1[(m-1)/2];		
		Fnplus1[(m-1)/2]=Q*(1+cos(2*M_PI*f*(k+1)*deltat-M_PI));
		for(int i=1;i<m-1;i++){
			Tnmoins1[i]=Tn[i];
			Tn[i]=Tnplus1[i];
			Unmoins1[i]=Un[i];
			Un[i]=Unplus1[i];
		}
	}
}