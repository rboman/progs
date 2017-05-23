%          ----------------------------------
%          Calcul des accelerations (+graphe)
%          ----------------------------------
%          

a1=1.5; a2=5; a3=3; xb=4.5;
ya=3; L=10.5; e=1; dp=0.5;
theta1p=100*pi/3;

for i=1:73,  
  j=[-a2*sin(x1(i)) -a3*sin(x2(i)); a2*cos(x1(i)) a3*cos(x2(i))];
  dj=[-a2*cos(x1(i))*theta2p(i) -a3*cos(x2(i))*theta3p(i); -a2*sin(x1(i))*theta2p(i) -a3*sin(x2(i))*theta3p(i)];
  cp=[a1*cos(theta1(i))*theta1p; a1*sin(theta1(i))*theta1p];
  x=inv(j)*(cp*theta1p-dj*[theta2p(i); theta3p(i)]);

  theta2pp(i)=x(1,1);
  theta3pp(i)=x(2,1);

  xppp(i)=-a1*cos(theta1(i))*(theta1p)^2-L*cos(x1(i))*(theta2p(i))^2-L*sin(x1(i))*theta2pp(i)-dp*sin(x1(i))*(theta2p(i))^2+dp*cos(x1(i))*theta2pp(i);
  yppp(i)=-a1*sin(theta1(i))*(theta1p)^2-L*sin(x1(i))*(theta2p(i))^2+L*cos(x1(i))*theta2pp(i)+dp*cos(x1(i))*(theta2p(i))^2+dp*sin(x1(i))*theta2pp(i);
end

plot(theta1,theta2pp,theta1,theta3pp)
title('Derivees secondes de theta2&3 en fct de theta1')
xlabel('theta1 [rad]')
ylabel('d^2theta2/dt^2  d^2theta3/dt^2   [rad/s^2]')
grid
pause

plot(theta1,xppp,theta1,yppp)
title('Derivees secondes de Xp et Yp en fct de theta1')
xlabel('theta1 [rad]')
ylabel('d^2Xp/dt^2  d^2Yp/dt^2   [cm/s^2]') 
grid
pause
