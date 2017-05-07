%     --------------------------------------------------------
%     Calcul des deux points o— xpp=0 + r‚duction du m‚canisme
%     --------------------------------------------------------

a1=1.4; a2=5; a3=3; xb=4.9255;    %--Dim. du m‚canisme ayant subi la
ya=2.2337; L=9; e=1; dp=0.8;      %  rotation

%--Calcul du premier pt o— xpp=0 
e2=0;
theta1(1)=6;
theta1(2)=2*pi;
theta1(3)=(theta1(1)+theta1(2))/2;
for i=1:3,
   %--Estimations initiales.         
   x1(i)=353*pi/180;            
   x2(i)=230*pi/180;
end
calcule
xmax=xextrem;
e=e2;                               %--Ajuste la valeur de e … la nouvelle trajectoire
thetamax=textrem                    

%--Calcul du deuxiŠme pt o— xpp=0 
theta1(1)=2.9;
theta1(2)=3.3;
theta1(3)=(theta1(1)+theta1(2))/2;
for i=1:3,
   %--Estimations initiales.
   x1(i)=6.3;              
   x2(i)=5.2;
end
calcule
xmin=xextrem;
thetamin=textrem


longueur=xmax-xmin;                 %--Distance entre les deux pts
facteur=longueur/1.9                %--Facteur d'‚chelle
a1=a1/facteur;                      %--R‚duction du m‚canisme
a2=a2/facteur;
a3=a3/facteur;
L=L/facteur;
dp=dp/facteur;
ya=ya/facteur;
xb=xb/facteur;
e=e/facteur;
[a1 a2 a3 L dp ya xb e]             %--Affichage des r‚sultats

temps=(thetamax-thetamin)/(48*pi)   %--Calcul du temps d'exposition


