%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%               Projet MICO - Riks Crisfield (v6.0)
%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

% ----Donnees-------------------------------------

clear
h=40; a=24; b=40; Vh=160; Vv=200;
E=70000; Lh=2*a; Lv=sqrt(h^2+(b+a)^2);
Ah=Vh/Lh; Av=Vv/Lv;
Beta=1e-2;
TOL=1e-3;

% ----Calcul de LaCR------------------------------
% ...

% ----Initialisation------------------------------

R=[20000,0]; Force=0;
t=1; uu=[0,0]; 
x(1)=0; y(1)=0; Force(1)=0;
lambda=0.005;

% ----1° pas de temps: N-R normal avec P=100------

t=t+1;
boucle=0;
u=uu(1); v=uu(2);
% ----F(u,v)--------------
F(1)=(E*Vv/Lv^4)*(v*v+u*u-2*h*v+2*u*(a+b))*(v-h);
F(2)=(Vv/Lv^4)*(v^2+u^2-2*h*v+2*u*(a+b))*(u+a+b);
F(2)=F(2)+(8*Vh/Lh^4)*(u^2+2*a*u)*(u+a);
% ----Matrice tg----------
Kt(1,1)=E*Vv/(Lv^4)*(2*u+2*(a+b))*(v-h);
Kt(1,2)=E*Vv/(Lv^4)*((v^2+u^2-2*v*h+2*u*(a+b))+(v-h)*(2*v-2*h));
Kt(2,1)=Vv/(Lv^4)*((v^2+u^2-2*h*v+2*u*(a+b))+(u+a+b)*(2*u+2*(a+b)));
Kt(2,1)=Kt(2,1)+8*Vh/(Lh^4)*((2*u+2*a)*(u+a)+(u^2+2*a*u));
Kt(2,2)=Vv/(Lv^4)*(u+a+b)*(2*v-2*h);
Du1=(inv(Kt)*(lambda*R-F)')';
uu=uu+Du1;
while boucle==0
   u=uu(1); v=uu(2);
   % ----F(u,v)--------------
   F(1)=(E*Vv/Lv^4)*(v*v+u*u-2*h*v+2*u*(a+b))*(v-h);
   F(2)=(Vv/Lv^4)*(v^2+u^2-2*h*v+2*u*(a+b))*(u+a+b);
   F(2)=F(2)+(8*Vh/Lh^4)*(u^2+2*a*u)*(u+a);
   if norm(lambda*R-F)/norm(lambda*R)>10E-3
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
x(t)=uu(2);
y(t)=uu(1);
Force(t)=lambda*R(1);

% ----Pas de temps suivants-----------------------

ETA=norm(Du1)/norm(uu);
lambda=Force(t)/R(1);

while lambda<1
   i=1;
   lambdat=lambda;
   uut=uu;
   lambda=(Force(t)+ETA/Beta)/R(1);
   boucle=0;
   while boucle==0
      u=uu(1); v=uu(2);
      % ----Fint(u,v)--------------
      Fint(1)=(E*Vv/Lv^4)*(v*v+u*u-2*h*v+2*u*(a+b))*(v-h);
      Fint(2)=(E*Vv/Lv^4)*(v^2+u^2-2*h*v+2*u*(a+b))*(u+a+b);
      Fint(2)=Fint(2)+(8*E*Vh/Lh^4)*(u^2+2*a*u)*(u+a);
      if norm((Fint-lambda*R)/(lambda*R))>TOL
         i=i+1;
         % ----Matrice tg-------------
         Kt(1,1)=E*Vv/(Lv^4)*(2*u+2*(a+b))*(v-h);
         Kt(1,2)=E*Vv/(Lv^4)*((v^2+u^2-2*v*h+2*u*(a+b))+(v-h)*(2*v-2*h));
         Kt(2,1)=E*Vv/(Lv^4)*((v^2+u^2-2*h*v+2*u*(a+b))+(u+a+b)*(2*u+2*(a+b)));
         Kt(2,1)=Kt(2,1)+8*E*Vh/(Lh^4)*((2*u+2*a)*(u+a)+(u^2+2*a*u));
         Kt(2,2)=E*Vv/(Lv^4)*(u+a+b)*(2*v-2*h);
         % ----Calcul de Du1-----------
         Du1=(Kt\(lambda*R-Fint)')';
         % ----Calcul de Du2-----------
         Du2=(Kt\R')';
         a1=Beta^2*R*(R')+Du2*(Du2');
         a2=2*((lambda-lambdat)*Beta^2*R*(R')+(uu-uut+Du1)*(Du2'));
         a3=2*(uu-uut)*(Du1')+Du1*(Du1')+((uu-uut)*((uu-uut)')+(lambda-lambdat)^2*Beta^2*R*(R')-ETA^2);
         rac1=(-a2+sqrt(a2^2-4*a1*a3))/2/a1;
         rac2=(-a2-sqrt(a2^2-4*a1*a3))/2/a1;
         test=uu+Du1+rac2*Du2;
         if test(2)-uut(2)>0
            uu=uu+Du1+rac2*Du2;
            lambda=lambda+rac2;
         else
            uu=uu+Du1+rac1*Du2;
            lambda=lambda+rac1;
         end         
      else
         boucle=1;
      end
   end
   t=t+1;
   Force(t)=lambda*R(1);
   x(t)=uu(2);
   y(t)=uu(1);
   ETA=ETA*sqrt(4/i);
   if ETA>5 
      ETA=5;
   end
end

% ----Graphes-------------------------------------

figure(1)
plot(x,y), xlabel('v [mm]'), ylabel('u [mm]'), grid
figure(2)
plot(x,Force), xlabel('v [mm]'), ylabel('P [N]'), grid

