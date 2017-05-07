eta=cond/rhoc;
a1=cond*A/dx;
a2=rhoc*A*dx/6;
for i=2:N
   K(i,i)   = 2*a1;
   K(i,i-1) = -a1;
   K(i,i+1) = -a1;
   C(i,i)   = 4*a2;
   C(i,i-1) = a2;
   C(i,i+1) = a2;  
end
K(1,1)     = a1 +h*A;
K(1,2)     = -a1;
K(N+1,N+1) = a1;
K(N+1,N)   = -a1;
C(1,1)     = 2*a2;
C(1,2)     =   a2;
C(N+1,N+1) = 2*a2;
C(N+1,N)   =   a2;

vp=eig(inv(C)*K);
alpmc=max(vp)
alpm2=12*eta/dx^2
dtmo=1/(1-theta)/alpmc
dtms=2/(1-2*theta)/alpmc