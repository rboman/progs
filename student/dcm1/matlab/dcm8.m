%          ----------------------------------------
%          Calcul de l'angle de rotation et ya2,xb2 
%          ----------------------------------------                         
%                    (Par NEWTON-RAPHSON)


f=[1:5]';
betamin=100;
epsilon=10^(-10);                 %--Pr‚cision des r‚sultats.
x1=9; x2=0.7; x3=1.9;             %--Estimations initiales.
x4=6; x5=4.8;
theta1p=48*pi;                    %--Vitesse de rotation de la manivelle

for i=1:300,
   g=[0;-ya;yp(i)+xpp(i)/ypp(i)*xp(i);xb;-ya];
   f(1,1)=-x1+a1*cos(x3)+L*cos(x4)+L*sin(x4);
   f(2,1)=-x2+a1*sin(x3)+L*sin(x4)-dp*cos(x4);   
   f(3,1)=x2+xpp(i)/ypp(i)*x1;
   f(4,1)=a1*cos(x3)+a2*cos(x4)+a3*cos(x5);
   f(5,1)=a1*sin(x3)+a2*sin(x4)+a3*sin(x5);
   r=g-f;
   x=[x1;x2;x3;x4;x5];
   while norm(r)>epsilon,
      j=[1:5];
      j(1,:)=[-1 0 -a1*sin(x3) -L*sin(x4)+L*cos(x4) 0];
      j(2,:)=[0 -1 a1*cos(x3) L*cos(x4)+dp*sin(x4) 0];
      j(3,:)=[xpp(i)/ypp(i) 1 0 0 0];
      j(4,:)=[0 0 -a1*sin(x3) -a2*sin(x4) -a3*sin(x5)];
      j(5,:)=[0 0 a1*cos(x3) a2*cos(x4) a3*cos(x5)];
      x=x+inv(j)*r;
      x1=x(1);x2=x(2);x3=x(3);x4=x(4);x5=x(5);
      g=[0;-ya;yp(i)+xpp(i)/ypp(i)*xp(i);xb;-ya];
      f(1,1)=-x1+a1*cos(x3)+L*cos(x4)+L*sin(x4);
      f(2,1)=-x2+a1*sin(x3)+L*sin(x4)-dp*cos(x4);   
      f(3,1)=x2+xpp(i)/ypp(i)*x1;
      f(4,1)=a1*cos(x3)+a2*cos(x4)+a3*cos(x5);
      f(5,1)=a1*sin(x3)+a2*sin(x4)+a3*sin(x5);
      r=g-f;
   end

   %--Calcul de xqp, yqp, xq, yq
   j=[a2*sin(x4) a3*sin(x5); a2*cos(x4) a3*cos(x5)];
   c=[-a1*sin(x3); -a1*cos(x3)];
   c=c*theta1p;
   xx=j\c;
   theta2p=xx(1,1);
   xqp(i)=-a1*sin(x3)*theta1p-(L*sin(x4)-dp*cos(x4))*theta2p;
   yqp(i)=a1*cos(x3)*theta1p+(L*cos(x4)+dp*sin(x4))*theta2p;
   xq(i)=a1*cos(x3)+L*cos(x4)+dp*sin(x4);
   yq(i)=ya+a1*sin(x3)+L*sin(x4)-dp*cos(x4);
   beta(i)=abs(atan(yqp(i)/xqp(i))-atan(ypp(i)/xpp(i)));
   if beta(i)<betamin,
      betamin=beta(i)
      res=i;
   end
end

%--Dessine la variation de beta (angle entre les deux tangentes)
%  Pour pouvoir r‚duire l'intervalle initial.
plot(theta1,beta)
grid
pause

%--Calcule l'angle de rotation optimal 
alpha=atan(-xpp(res)/ypp(res))   

%--Calcule les nouvelles valeurs de ya et xb
gamma=atan(ya/xb);                
racine=sqrt(xb^2+ya^2);
xb2=racine*cos(gamma+alpha)
ya2=racine*sin(gamma+alpha)
