%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%                  Projet MICO - Contraintes
%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

% ----Donnees-------------
h=40; a=24; b=40; Vh=160; Vv=200; E=70000;
Lh=2*a; Lv=sqrt(h^2+(b+a)^2);
Ah=Vh/Lh; Av=Vv/Lv;
TOL=1e-5;

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
   P(i)=(E*Vv/Lv^4)*(v*v+u*u-2*h*v+2*u*(a+b))*(v-h);
   sigh(i)=P(i)*(u+a)*(u+b+a)/((h-v)*Vh);
   Th(i)=P(i)*a*(u+b+a)/((h-v)*Vh);
   Sh(i)=E*u*(u+2*a)/(2*a^2);
   Tv(i)=P(i)*sqrt((a+b+u)^2+(h-v)^2)*sqrt((a+b)^2+h^2)/(2*Vv*(v-h));
   Sv(i)=E*(2*u*(a+b)+u^2-2*h*v+v^2)/(2*(a+b)^2+2*h^2);
   sigv(i)=P(i)*((a+b+u)^2+(h-v)^2)/(2*Vv*(v-h));
end

% ----Graphes-------------
figure(1)
plot(x,sigh,'k',x,Sh,'k',x,Th,'k'), xlabel('v [mm]'),title('Barre horizontale'), grid
figure(2)
plot(x,sigv,'k',x,Sv,'k',x,Tv,'k'), xlabel('v [mm]'),title('Barre oblique'), grid

