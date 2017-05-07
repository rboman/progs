res,uan
ua=flipud(ua);
%
% solution complète
%
plot(0:dx:L,u,'y-',0:dx:L,ua,'b-')
title('Profil des températures au cours du temps')
xlabel('X [m]')
%
%  q(x=dx/2)
%
for i=1:size(u,1)-1
   q(i,:)=(u(i,:)-u(i+1,:))*cond/dx;
   qa(i,:)=(ua(i,:)-ua(i+1,:))*cond/dx;
end
figure(2)
plot(t,q(1,:),'y-',t2,qa(1,:),'b-')
title('Flux thermique en x=dx/2')
xlabel('temps [s]')
%
%  T(x=L)
%
figure(3)
plot(t,u(size(u,1),:),'y-',t2,ua(size(u,1),:),'b-')
title('Température en x=L')
xlabel('temps [s]')
%
%  T(x=0)
%
figure(4)
tt=[];
co=t(2)/2;
for i=1:size(ua,2)-1
   Tfil(i)=(u(1,i)+u(1,i+1))/2;
   tt=[tt co];
   co=co+(t(2)-t(1));
end
plot(t,u(1,:),'y-',t2,ua(1,:),'b-',tt,Tfil,'g-')
title('Température en x=0')
xlabel('temps [s]')
figure(5)
plot(t(1:15),u(1,1:15),'y-',t2(1:15),ua(1,1:15),'b-',tt(1:14),Tfil(1:14),'g-')
title('Température en x=0 (Zoom près de t=0)')
xlabel('temps [s]')
%
%  erreur T(x=0)
%
figure(6)
plot(t(1:size(ua,2)),(ua(1,:)-u(1,1:size(ua,2)))/max(ua(1,:)))
title('Erreur relative sur la température en x=0')
xlabel('temps [s]')
%
%
%
figure(7)
plot(0:dx:L,u(:,2),'y-',0:dx:L,ua(:,2),'b-')
title('Profil des températures en t = dt')
xlabel('X [m]')