%          -------------------------------------------------
%          Calcul des deux points de la trajectoire o— xpp=0
%          -------------------------------------------------                    


epsilon=10^(-10);               %--Pr‚cision des resultats.
theta1p=48*pi;

while((theta1(2)-theta1(1))>epsilon),
for i=1:3,
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

   xp(i)=a1*cos(theta1(i))+L*cos(x1(i))+dp*cos(x1(i)-pi/2);
   yp(i)=ya+a1*sin(theta1(i))+L*sin(x1(i))+dp*sin(x1(i)-pi/2);

   j=[a2*sin(x1(i)) a3*sin(x2(i)); a2*cos(x1(i)) a3*cos(x2(i))];
   c=[-a1*sin(theta1(i)); -a1*cos(theta1(i))];
   c=c*theta1p;
   xx=j\c;
   
   theta2p(i)=xx(1,1);
   theta3p(i)=xx(2,1);
   
   xpp(i)=-a1*sin(theta1(i))*theta1p-(L*sin(x1(i))+dp*sin(x1(i)-pi/2))*theta2p(i);
   ypp(i)=a1*cos(theta1(i))*theta1p+(L*cos(x1(i))+dp*cos(x1(i)-pi/2))*theta2p(i);
end
if xpp(1)*xpp(3)>0,
   theta1(1)=theta1(3);
else
   theta1(2)=theta1(3);
end
theta1(3)=(theta1(1)+theta1(2))/2;
end

e2=yp(3);
xextrem=xp(3);
textrem=theta1(3);
