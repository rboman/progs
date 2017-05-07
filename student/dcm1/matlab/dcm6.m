%          ---------------------------------------------------------
%          Calcul de theta2&3 dans un intervalle donn‚ (300 valeurs)
%          ---------------------------------------------------------
%                         (Par NEWTON-RAPHSON)


a1=1.4; a2=5; a3=3; xb=4.5;    %--Dim. du m‚canisme.
ya=3; L=9; e=1; dp=0.8;

epsilon=10^(-10);              %--Pr‚cision des r‚sultats.
x1(1)=353*pi/180;              %--Estimations initiales.
x2(1)=230*pi/180;

for i=1:300,
   theta1(i)=-0.4+i/300*0.25;
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
   if i<300,
      x1(i+1)=x1(i);
      x2(i+1)=x2(i);
   end
   %--Calcul de la position de P en fct des angles.
   xp(i)=a1*cos(theta1(i))+L*cos(x1(i))+dp*cos(x1(i)-pi/2);
   yp(i)=ya+a1*sin(theta1(i))+L*sin(x1(i))+dp*sin(x1(i)-pi/2);
end
