%          ----------------------------------------------------
%          Calcul de theta2 & theta3 en fct de theta1 (+graphe)
%          ----------------------------------------------------
%              M‚thode analytique: (ATAN2 - POLY_2D - ASIN)


a1=1.5; a2=5; a3=3; xb=4.5;    %--Dim. du m‚canisme.
ya=3; L=10.5; e=1; dp=0.5;
x1=x1';                        %--Transpose les vecteurs theta2 & theta3 
x2=x2';                        %--pour pouvoir comparer les r‚sultats

for i=1:73,
   theta1(i)=(i-1)*5/180*pi;
   k1=xb-a1*cos(theta1(i));
   k2=-ya-a1*sin(theta1(i));
   k3=(k1^2+k2^2+a2^2-a3^2)/(2*a2);
   k4=(k1^2+k2^2+a3^2-a2^2)/(2*a3);
   ro=sqrt(k1^2+k2^2);
   phi=atan2(k1,k2);

   %--M‚thode ATAN2:
   x1(i,2)=atan2(k3/ro,-sqrt(1-k3^2/ro^2))-phi+2*pi;
   x2(i,2)=atan2(k4/ro,sqrt(1-k4^2/ro^2))-phi+2*pi;

   %--M‚thode POLY_2D:
   x1(i,3)=2*atan((k2+sqrt(k2^2-(k3+k1)*(k3-k1)))/(k3+k1))+2*pi;
   x2(i,3)=2*atan((k2-sqrt(k2^2-(k4+k1)*(k4-k1)))/(k4+k1))+2*pi;  
   
   %--M‚thode ASIN:
   x1(i,4)=3*pi-asin(k3/ro)-phi;
   x2(i,4)=2*pi+asin(k4/ro)-phi;   
   
   %--Calcul de la position de P en fct des angles.
   xp(i)=a1*cos(theta1(i))+L*cos(x1(i))+dp*sin(x1(i));
   yp(i)=ya+a1*sin(theta1(i))+L*sin(x1(i))-dp*cos(x1(i));
end


%--Graphiques--:

plot(theta1,x1(:,2),theta1,x2(:,2))
title('theta2 & theta3 en fct. de theta1 (ATAN2)')
xlabel('theta1 [rad]')
ylabel('theta [rad]')
grid
pause

plot(theta1,x1(:,3),theta1,x2(:,3))
title('theta2 & theta3 en fct. de theta1 (EQU EN t DU 2 DEGRE)')
xlabel('theta1 [rad]')
ylabel('theta [rad]')
grid
pause

plot(theta1,x1(:,4),theta1,x2(:,4))
title('theta2 & theta3 en fct. de theta1 (ASINUS)')
xlabel('theta1 [rad]')
ylabel('theta [rad]')
grid
pause
