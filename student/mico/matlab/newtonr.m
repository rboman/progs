%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%        Projet MICO - Newton Raphson (v2.0)    21.03.95 
%
%   . Utilisation de lambda
%   . Test sur les Forces
%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

% ----Donnees-------------

h=40; a=24; b=40; Vh=160; Vv=200;
E=70000; Lh=2*a; Lv=sqrt(h^2+(b+a)^2);
Ah=Vh/Lh; Av=Vv/Lv;

TOL=1e-3;

i=0; x=0; y=0;
Force=0; lambda=0;
uu=[0,0]; R=[9000,0];
 
% ----Boucle principale----

for lambda=0:0.01:1
   i=i+1; boucle=0;
   while boucle==0
      u=uu(1); v=uu(2);
      % ----F(u,v)--------------
      F(1)=(E*Vv/Lv^4)*(v*v+u*u-2*h*v+2*u*(a+b))*(v-h);
      F(2)=(Vv/Lv^4)*(v^2+u^2-2*h*v+2*u*(a+b))*(u+a+b);
      F(2)=F(2)+(8*Vh/Lh^4)*(u^2+2*a*u)*(u+a);
      if norm((lambda*R-F)/(lambda*R))>TOL
         % ----Matrice tg----------
         Kt(1,1)=E*Vv/(Lv^4)*(2*u+2*(a+b))*(v-h);
         Kt(1,2)=E*Vv/(Lv^4)*((v^2+u^2-2*v*h+2*u*(a+b))+(v-h)*(2*v-2*h));
         Kt(2,1)=Vv/(Lv^4)*((v^2+u^2-2*h*v+2*u*(a+b))+(u+a+b)*(2*u+2*(a+b)));
         Kt(2,1)=Kt(2,1)+8*Vh/(Lh^4)*((2*u+2*a)*(u+a)+(u^2+2*a*u));
         Kt(2,2)=Vv/(Lv^4)*(u+a+b)*(2*v-2*h);
         Du=(inv(Kt)*(lambda*R-F)')';
         uu=uu+Du;
      else
         boucle=1;
      end
   end
   x(i)=uu(2);
   y(i)=uu(1);
   Force(i)=lambda*R(1);
end

% ----Graphes-------------

figure(1)
plot(x,y), xlabel('v [mm]'), ylabel('u [mm]'), grid
figure(2)
plot(x,Force), xlabel('v [mm]'), ylabel('P [N]'), grid
end