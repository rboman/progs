N=size(a);
h0=100; g=0.01; f=1e-4; U=0.1;
L=20000;

dh=f*L*U/g;

Epot(1)=NaN; Ecin(1)=NaN;

j=1;
e1=ones(1,N(2));
e2=e1';
e2(1,1)=0.5; e2(N(2),1)=0.5;

for i=2:N(1)/N(2)
   j=j+1;
   h=a((i-1)*N(2)+1:i*N(2),:)*dh+h0;
   h(N(2),:)=zeros(1,N(2)); h2=h.*h;
   Epot(j)=e1*h2*e2*g;

   uu=u((i-1)*N(2)+1:i*N(2),:)*U;
   vv=v((i-1)*N(2)+1:i*N(2),:)*U;
   Ecin(j)=e1*((uu.*uu+vv.*vv).*h)*e2;
end

figure(1), plot(1:j,Epot,'k')
title('Energie potentielle (à un facteur près)')
xlabel('temps (/2)')
figure(2), plot(1:j,Ecin,'k')
title('Energie cinétique (à un facteur près)')
xlabel('temps (/2)')
Ep_rel=(max(Epot(2:j))-min(Epot(2:j)))/min(Epot(2:j))
Ec_rel=(max(Ecin(2:j))-min(Ecin(2:j)))/min(Ecin(2:j))

