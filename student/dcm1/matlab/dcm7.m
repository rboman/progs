%                 ---------------------------------------
%                 Calcul des vitesses pour l'optimisation
%                 ---------------------------------------

theta1p=48*pi;              %--D‚riv‚e de theta1.

for i=1:300,
   j=[a2*sin(x1(i)) a3*sin(x2(i)); a2*cos(x1(i)) a3*cos(x2(i))];
   c=[-a1*sin(theta1(i)); -a1*cos(theta1(i))];
   c=c*theta1p;
   x=j\c;
   
   theta2p(i)=x(1,1);
   theta3p(i)=x(2,1);
   
   xpp(i)=-a1*sin(theta1(i))*theta1p-(L*sin(x1(i))+dp*sin(x1(i)-pi/2))*theta2p(i);
   ypp(i)=a1*cos(theta1(i))*theta1p+(L*cos(x1(i))+dp*cos(x1(i)-pi/2))*theta2p(i);
end

