%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%                 Projet MICO - Partie théorique
%
%  . Calcule u(v) et P(u) par Newton Raphson
%  . Vérifie l'ordre de grandeur du résultat
% 
%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

% ----Donnees-------------
h=40; a=24; b=40; Vh=160; Vv=200; E=70000;
Lh=2*a; Lv=sqrt(h^2+(b+a)^2);
Ah=Vh/Lh; Av=Vv/Lv;
TOL=1e-5;

% ---Vérification---------
Force=(2*b-2*Lv+Lh)/(-2*Lv/E/Av-Lh/E/Ah);
Lvd=Lv*(-Force/E/Av+1);
Lhd=Lh*(Force/E/Ah+1);
umax=Lvd-b-a;

% ----Trace u fct. de v---
i=0; u=0; x=0; y=0; lastu=0;

for v=0:1:100
   i=i+1; boucle=0;
   lastu=0;
   F=(Vv/Lv^4)*(v^2+u^2-2*h*v+2*u*(a+b))*(u+a+b);
   F=F+(8*Vh/Lh^4)*(u^2+2*a*u)*(u+a);
   DF=(Vv/Lv^4)*((u+a+b)*(2*u+2*(a+b))+(v^2+u^2-2*h*v+2*u*(a+b)));
   DF=DF+(8*Vh/Lh^4)*((2*u+2*a)*(u+a)+(u^2+2*u*a));
   Du=-F/DF; u=u+Du;
   while boucle==0
      F=(Vv/Lv^4)*(v^2+u^2-2*h*v+2*u*(a+b))*(u+a+b);
      F=F+(8*Vh/Lh^4)*(u^2+2*a*u)*(u+a);
      if abs(u-lastu)>TOL
         DF=(Vv/Lv^4)*((u+a+b)*(2*u+2*(a+b))+(v^2+u^2-2*h*v+2*u*(a+b)));
         DF=DF+(8*Vh/Lh^4)*((2*u+2*a)*(u+a)+(u^2+2*u*a));
         Du=-F/DF; lastu=u; u=u+Du;
      else
         boucle=1;
      end
   end
   x(i)=v;
   y(i)=u;
   Dfu=E*Vv/(Lv^4)*(2*(a+b))*(-h);
   Dfv=E*Vv/(Lv^4)*((-h)*(-2*h));
   Plin(i)=u*Dfu+v*Dfv;
   P(i)=(E*Vv/Lv^4)*(v*v+u*u-2*h*v+2*u*(a+b))*(v-h);
end

% ----Graphes-------------
figure(1)
plot(x,y), xlabel('v [mm]'), ylabel('u [mm]'), grid
figure(2)
plot(x,P), xlabel('v [mm]'), ylabel('P [N]'), grid
figure(3)
plot(x,Plin), xlabel('v [mm]'), ylabel('P (lin) [N]'), grid
