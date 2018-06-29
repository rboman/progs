%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%      Analyse.m : effectue une analyse des resultats
%
% derniere modification : 02.01.97 
%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

res                   % lecture du fichier resultat
itmax=size(X,2)-1     % Nbre d'iterations
X(:,itmax+1)

figure(1)
plot(0:itmax,FCT)
xlabel('iterations'), ylabel('F(x)')
title('Decroissance de la fonction objectif')

figure(2)
plot(0:itmax,G)
xlabel('iterations'), ylabel('|g(x)|')
title('Decroissance de la norme du gradient')

figure(3)
plot(0:itmax,X','k-')
xlabel('iterations'), ylabel('Xi')
title('Convergence vers le minimum')