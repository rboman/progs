H=h/cond;
eta=cond/rhoc;
%Tf=20;
%L=1.0;
NT=10;
NX=N;
dt=10*dt;
dx=L/NX;
%dt=0.005;

global H L
NZ=20;
tol=1D-06;

%     Recherche des zeros
%     -------------------
disp('Calcul des racines')
for i=1:NZ
   x1=(i-1+0.001)*pi/L;
   x2=(i-1-0.001)*pi/L+pi/L/2.;
   f1=fct(x1);
   while (abs(f1)>tol)
      x3=(x2+x1)/2;
      f3=fct(x3);
      if(f3*f1>0)
         x1=x3;
         f1=fct(x1);
      else
         x2=x3;
      end
%      [x1 x2 f1]
   end
   x1=(x1+x2)/2;
%   pause
   bm(i)=x1;
   bm2(i)=i*pi/L;
end
%
%     Calcul de la solution analytique (montée)
%     -----------------------------------------
for k=1:NT
   k
   t(k)=(k-1)*dt;
   for j=1:NX+1
      x(j)=(j-1)*dx;
      sum=0;
      for i=1:NZ
         sum=sum+1/(L*(bm(i)^2+H^2)+H)*cos(bm(i)*x(j))/cos(bm(i)*L)*exp(-bm(i)^2*eta*t(k));
      end
      ua(j,k)=Tf-2*Tf*H*sum;
   end
end
%
%     Calcul de la solution analytique (descente)
%     -------------------------------------------
for k=NT+1:2*NT
   k
   t(k)=(k-1)*dt;
   for j=1:NX+1
      x(j)=(j-1)*dx;
%     Calcul somme sur n
      ua(j,k)=0;
%        calcul de C0
         C0=0;
         for i=1:NZ        
            C0=C0+1/(L*(bm(i)^2+H^2)+H)*exp(-(bm(i))^2*eta*(NT)*dt)/cos(bm(i)*L)*sin(bm(i)*L)/bm(i);
         end
         C0=Tf+C0*(-2*Tf*H*1/L);
         ua(j,k)=ua(j,k)+C0;
      for n=1:20
%        Calcul du Cn
         Cn=0;
         for i=1:NZ
            Cn=Cn+1/(L*(bm(i)^2+H^2)+H)*exp(-(bm(i))^2*eta*(NT)*dt)/cos(bm(i)*L)*(-1)^(n+1)*bm(i)*(L/n/pi)^2*sin(bm(i)*L)/(1-(bm(i)*L/n/pi)^2);
         end
         Cn=Cn*(-2*Tf*H*2/L);
         ua(j,k)=ua(j,k)+Cn*cos(n*pi*x(j)/L)*exp(-(n*pi/L)^2*eta*(t(k)-NT*dt));
      end
   end
end  
ua=flipud(ua);