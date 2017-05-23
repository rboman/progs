%          ------------------------
%          Graphe des vitesses (3D)
%          ------------------------


a1=1.5; a2=5; a3=3; xb=4.5;    %--Dim. du mecanisme.
ya=3; L=10.5; e=1; dp=0.5;
epsilon=10^(-10);              %--Precision des resultats.
x1(1)=353*pi/180;              %--Estimations initiales.
x2(1)=230*pi/180;
theta1p=100*pi/3;              %--Deriv‚e de theta1.

for i=1:730,
   theta1(i)=(i-1)*0.5/180*pi;
   g=[xb-a1*cos(theta1(i)) -ya-a1*sin(theta1(i))];
   f=[a2*cos(x1(i))+a3*cos(x2(i)) a2*sin(x1(i))+a3*sin(x2(i))];
   r=g-f;
   x=[x1(i); x2(i)];
   while norm(r)>epsilon,
      j=[-a2*sin(x(1,1)) -a3*sin(x(2,1)); a2*cos(x(1,1)) a3*cos(x(2,1))];
      x=x+inv(j)*r';
      g=[xb-a1*cos(theta1(i)) -ya-a1*sin(theta1(i))];
      f=[a2*cos(x(1,1))+a3*cos(x(2,1)) a2*sin(x(1,1))+a3*sin(x(2,1))];
      r=g-f;
   end
   x1(i)=x(1,1); x2(i)=x(2,1);
   %--Nouvelles estimations.
   if i<730,
      x1(i+1)=x1(i);
      x2(i+1)=x2(i);
   end
   %--Calcul de la position de P en fct des angles.
   xp(i)=a1*cos(theta1(i))+L*cos(x1(i))+dp*cos(x1(i)-pi/2);
   yp(i)=ya+a1*sin(theta1(i))+L*sin(x1(i))+dp*sin(x1(i)-pi/2);
end

%--Calcul des vitesses:

for i=1:730,
   j=[a2*sin(x1(i)) a3*sin(x2(i)); a2*cos(x1(i)) a3*cos(x2(i))];
   c=[-a1*sin(theta1(i)); -a1*cos(theta1(i))];
   c=c*theta1p;
   x=j\c;
   
   theta2p(i)=x(1,1);
   theta3p(i)=x(2,1);
   
   xpp(i)=-a1*sin(theta1(i))*theta1p-(L*sin(x1(i))+dp*sin(x1(i)-pi/2))*theta2p(i);
   ypp(i)=a1*cos(theta1(i))*theta1p+(L*cos(x1(i))+dp*cos(x1(i)-pi/2))*theta2p(i);
end

%--Graphique 3D:
graph=zeros(80);
for i=1:730,
graph((xp(i)-8.5)*20,(yp(i)-0.5)*20)=sqrt(xpp(i)^2+ypp(i)^2);
end
mesh(graph,[260,50])
pause

