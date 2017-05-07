c0=1.2;
c1=0.3;
l=22;
e=0.005;
rho=2700.0;
E=2e2;
F=1e-2;

compt=0;
for x=-l:1:0
  compt=compt+1; 
  h(compt)=(c1-c0)/l*(-x)+c0;
  m(compt)=F*rho*2*e*(11*h(compt)-2*e);
  i(compt)=E*(10*h(compt)^4/12-1/12*(10*h(compt)-2*e)*(h(compt)-2*e)^3);
end
for x=1:1:l
  compt=compt+1; 
  h(compt)=(c1-c0)/l*x+c0;
  m(compt)=F*rho*2*e*(11*h(compt)-2*e);
  i(compt)=E*(10*h(compt)^4/12-1/12*(10*h(compt)-2*e)*(h(compt)-2*e)^3);
end

plot(-l:1:l,h,'k',-l:1:l,m,'k',-l:1:l,i,'k');grid;
xlabel('x');
gtext('h(x)')
gtext('m(x)/100')
gtext('i(x)*200')
title('Evolution de la masse et de l''inertie');

  
