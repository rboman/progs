clear,res
plot(0:dx:L,u)
title('Profil des températures au cours du temps')
xlabel('X [m]')
C2=(T0-TL)/(1-exp(uu/cond*rhoc*L));
C1=T0-C2;

T=[];
for x=0:dx:L
   T=[T C1+C2*exp(uu/cond*rhoc*x)];
end

figure(2)
plot(0:dx:L,T,'-',0:dx:L,u(:,size(u,2)),'--',0:dx:L,u(:,size(u,2)),'+')
title('Comparaison avec la solution analytique stationnaire')
xlabel('X [m]')