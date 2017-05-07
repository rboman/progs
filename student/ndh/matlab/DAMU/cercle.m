% ********************************************************************
% *                  Travail Eléments aux Frontières                 *
% *                                                                  *
% *                       Le cercle de rayon R                       *
% ********************************************************************

% ********************************************************************
%     Hypothèse : Flux à déterminer sur chaque facette = constant
% ********************************************************************

clear,

flops(0);

R=1.2;     % Rayon du cercle
B=80.;     % BETA (en W/m^3)
K=400.;    % Coeff. de conductivité thermique (en W/m.K)
pi=acos(-1);

N=input('Nb de sous-domaines ?');
W=input('Nb d''intervalles pour l''intégration numérique ?');
P=input('Nb de points pour le calcul de T dans le domaine ? ');

% ------------------------------------
% Calcul de grandeurs caractéristiques
% ------------------------------------

tt=cputime;
theta=2*pi/N;          
L=2*R*sin(theta/2);
R1=R*cos(theta/2);

Phi=-B/(2*K)*R1^2;  % Température imposée par la condition limite

% -------------------
% Calcul de H et de G
% -------------------

  x_min=0.;        % initialisation de xmin et ymin
  y_min=L/2;

  H1(1)=0.;
  G1(1)=L/(2*pi)*(log(2/L)+1);

  for j=1:round((N-1)/2)   
  
     for n=0:W
 
      f1(n+1)=0;
      f2(n+1)=0;

      x=x_min-(n/W)*L*sin(j*theta);
      y=y_min+(n/W)*L*cos(j*theta);
      r=sqrt(x^2+y^2);

      f1(n+1)=-L/(2*pi*r^2)*(x*cos(j*theta)+y*sin(j*theta));
      f2(n+1)=L/(2*pi)*log(1/r);

    end,

    H1(j+1)=0.;
    G1(j+1)=0.;

    for n=1:W
     
       H1(j+1)=H1(j+1)+(f1(n)+f1(n+1))/(2*W);
       G1(j+1)=G1(j+1)+(f2(n)+f2(n+1))/(2*W);

    end,

    x_min=x_min-L*sin(j*theta);   % réactualistion de xmin et ymin
    y_min=y_min+L*cos(j*theta);

  end,

% -------------------------------------------
% Construction du reste des vecteurs H1 et G1
% -------------------------------------------

  if N/2==round(N/2)  % si vrai => N pair
     for c=1:N/2-1
         H1(N/2+c+1)=H1(N/2-c+1);
         G1(N/2+c+1)=G1(N/2-c+1); 
     end,
  else
     for c=1:(N-1)/2
         H1((N-1)/2+c+1)=H1((N-1)/2-c+2);
         G1((N-1)/2+c+1)=G1((N-1)/2-c+2);
     end,
  end,
 
% -------------------------------------
% Calcul des deux membres de l'équation
% -------------------------------------
  MG=0; % initialisation du membre de gauche
  
  for j=2:N
    MG=MG+H1(j); % somme de tous les éléments de H1 suf le premier qui est nul
  end,
 
  MG=MG+1/2;
  MG=MG*Phi;

  MD=0.; % initialisation du membre de droite

  for j=1:N
      MD=MD+G1(j); % somme de tous les éléments de G1
  end,

% --------------------------
% Calcul du vecteur des flux
% --------------------------

  Q=MG/MD,
  
% ------------------------
% Calcul de la température 
% ------------------------ 
  for k=0:P-1
 
      rr(k+1)=k*R/P;     % position du point sur le rayon
      x_min=R-rr(k+1);
      y_min=0.;
      
      for j=1:round(N/2)
 
        for n=0:W
 
          f1(n+1)=0;
          f2(n+1)=0;

          x=x_min-(n/W)*L*sin(theta/2+(j-1)*theta);
          y=y_min+(n/W)*L*cos(theta/2+(j-1)*theta);
          r=sqrt(x^2+y^2);
  
          f1(n+1)=-L/(2*pi*r^2)*(x*cos(theta/2+(j-1)*theta)+y*sin(theta/2+(j-1)*theta));
          f2(n+1)=L/(2*pi)*log(1/r);

        end,

        H2(j)=0.;
        G2(j)=0.;

        for n=1:W
     
         H2(j)=H2(j)+1/W*(f1(n)+f1(n+1))/2;
         G2(j)=G2(j)+1/W*(f2(n)+f2(n+1))/2;

        end,

      x_min=x_min-L*sin(theta/2+(j-1)*theta);
      y_min=y_min+L*cos(theta/2+(j-1)*theta);

      end,

      % Construction du reste des vecteurs H2 et G2
      % -------------------------------------------
 
         if N/2==round(N/2)
               for c=1:N/2
                   H2(N/2+c)=H2(N/2-c+1);
                   G2(N/2+c)=G2(N/2-c+1);
               end,
         else 
               for c=1:N-round(N/2)
                   H2(round(N/2)+c)=H2(round(N/2)-c);
                   G2(round(N/2)+c)=G2(round(N/2)-c);
               end,
         end,

      F(k+1)=0.;
 
      for j=1:N
      
        F(k+1)=F(k+1)+G2(j)*Q-H2(j)*Phi; 

      end,

      T_approx(k+1)=F(k+1)+B/(2*K)*(rr(k+1))^2; % CV inverse pour trouver la solution du problème de laplace
      T(k+1)=B/(2*K)*((rr(k+1))^2-R^2);
      
   end,
   flops,

   err=0.;

   for k=1:P
   err=err+abs((T(k)-T_approx(k))/T(k));
   end,
   err=err/P;

tt=cputime-tt;

figure,
plot(rr,T),title(['Cercle avec ',num2str(N),' éléments => erreur relative = ',num2str(err*100),' %',' et Temps CPU = ',num2str(tt),' s']),
hold on,
plot(rr,T_approx,'*'),
hold on,
plot(rr,F,'+'),grid,
hold off,
figure,
plot(rr,abs(T-T_approx)),title('Différence entre solution analytique et solution Eléments Frontières'),
