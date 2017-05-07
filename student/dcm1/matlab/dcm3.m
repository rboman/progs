%                           -------------------
%                           Calcul des vitesses
%                           -------------------
%
% rem: calculer theta 1,2&3 avant de lancer le prog.

a1=1.5; a2=5; a3=3; xb=4.5;    %--Dim. du m‚canisme.
ya=3; L=10.5; e=1; dp=0.5;
theta1p=100*pi/3;              %--D‚riv‚e de theta1.

for i=1:73,
   j=[a2*sin(x1(i)) a3*sin(x2(i)); a2*cos(x1(i)) a3*cos(x2(i))];
   c=[-a1*sin(theta1(i)); -a1*cos(theta1(i))];
   c=c*theta1p;
   x=j\c;
   
   theta2p(i)=x(1,1);
   theta3p(i)=x(2,1);
   
   xpp(i)=-a1*sin(theta1(i))*theta1p-(L*sin(x1(i))+dp*sin(x1(i)-pi/2))*theta2p(i);
   ypp(i)=a1*cos(theta1(i))*theta1p+(L*cos(x1(i))+dp*cos(x1(i)-pi/2))*theta2p(i);
end

%--Graphique:
plot(theta1,theta2p,theta1,theta3p)
title('Derivees de theta2 et theta3 en fct. de theta1')
xlabel('theta1 [rad]')
ylabel('dtheta/dt [rad/s]')
grid
pause

plot(theta1,xpp,theta1,ypp)
title('Derivees de Xp et Yp en fct. de theta1')
xlabel('theta1 [rad]')
ylabel('dXp/dt  dYp/dt  [cm/s]')
grid
pause

polar(theta1,sqrt(xpp.^2+ypp.^2))
title('Vitesse absolue de P en fct. de theta1')
grid

