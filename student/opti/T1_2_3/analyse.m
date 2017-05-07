%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%      Analyse.m : effectue une analyse des résultats
%
% dernière modification : 02.01.97 
%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

res                   % lecture du fichier résultat
itmax=size(X,2)-1     % Nbre d'itérations
X(:,itmax+1)

figure(1)
plot(0:itmax,FCT)
xlabel('itérations'), ylabel('F(x)')
title('Décroissance de la fonction objectif')

figure(2)
plot(0:itmax,G)
xlabel('itérations'), ylabel('|g(x)|')
title('Décroissance de la norme du gradient')

figure(3)
plot(0:itmax,X','k-')
xlabel('itérations'), ylabel('Xi')
title('Convergence vers le minimum')