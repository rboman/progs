b=80;
a=1.2;
k=400;
s=0;
for n=1:2:100
   s=s+(32*b*a*a)/(pi^3*k)*(-1)^((n-1)/2)*(1-1/cosh(n*pi/2))*(n*pi/2/a)*sin(n*pi/2)/n^3;
end
s=s-b*a/k