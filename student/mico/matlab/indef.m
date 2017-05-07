%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%         Projet MICO - Barre horizontale indéformable
%
% 
%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

% ----Donnees-------------
h=40; a=24; b=40; Vh=160; Vv=200; E=70000;
Lh=2*a; Lv=sqrt(h^2+(b+a)^2);
Ah=Vh/Lh; Av=Vv/Lv;
P=0;

for v=0:90
   P(v+1)=Vv*E*v*(v-h)*(v-2*h)/Lv^4;
end

figure(1)
plot(0:90,P,'k'), xlabel('v [mm]'), grid
