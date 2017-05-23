%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%           Calcul de la réponse de l'avion lors de l'atterrissage
%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%----------Initialisation--------------------
vpvp
graph
compt=0;
YXT=0.0; 
alp0(1)=-2*yo(1)*F0*T/pi

%---------1° Phase: F=F0*sin(pi*t/T)---------

for temps=0:(T/np2):T
temps 
   compt=compt+1;
   for xx=1:1:np+1
      YXT(compt,xx)=grafic(xx,1)*(-yo(1)*F0*(T/pi)^2*sin(pi*temps/T)+(alp0(1)+yo(1)*F0*T/pi)*temps);
      for i=3:2:NMOD 
         YXT(compt,xx)=YXT(compt,xx)+grafic(xx,i)*((F0*yo(i)/vap(i)*(T/(pi)*...
            sin(vap(i)*temps)-vap(i)*T*T/(pi*pi)*sin(pi*temps/T))/...
            (1-(vap(i)*T/pi)^2))); 
      end
%      YXT(compt,xx)=YXT(compt,xx);   
   end
end

%---------2° Phase: plus de force--------------
alpha(1)=F0*yo(1)*T*T/pi+alp0(1)*T;      
alphap(1)=F0*yo(1)*T/pi*2+alp0(1);

for i=3:2:NMOD 
   alpha(i)=F0*yo(i)/vap(i)*(T/pi*sin(vap(i)*T))/...
            (1-(vap(i)*T/pi)^2);
   alphap(i)=F0*yo(i)/vap(i)...
             *(T/pi*vap(i)*cos(vap(i)*T)+vap(i)*T/pi)...
             /(1-(vap(i)*T/pi)^2);
end
      
for temps=(T+T/np2):(T/np2):(NPERIOD*T) 
temps
   compt=compt+1;
   for xx=1:1:np+1
      YXT(compt,xx)=grafic(xx,1)*(alphap(1)*(temps-T)+alpha(1));
      for i=3:2:NMOD 
         YXT(compt,xx)=YXT(compt,xx)+grafic(xx,i)*...
         (alpha(i)*cos(vap(i)*(temps-T))+1/vap(i)*...
         alphap(i)*sin(vap(i)*(temps-T))); 
      end
   end
end

%--------Affichage des résultats---------------

figure(2);
mesh(YXT'); grid;                    
title('Vue globale de l aile');
figure(3); 
plot(0:T/np2:NPERIOD*T,YXT(:,1)-YXT(:,np/2),'k'); grid;
title('Evolution relative du bout de l aile');
figure(4);
plot(0:T/np2:NPERIOD*T,YXT(:,np/2),'k'); grid;
title('Trajectoire du fuselage');
for t=1:1:compt
   YXT(t,:)=YXT(t,:)-YXT(t,np/2);
end
figure(5); 
mesh(YXT'); grid;
title('Déformation relative');
mt
