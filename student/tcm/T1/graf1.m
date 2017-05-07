
x=0;
dx=0.01;
N=1000;
for i=1:N+1
      y(i)=tan(x);
      z(i)=1/x;
      x=x+dx;
end
plot(0:dx:N*dx,y,0:dx:N*dx,z)
axis([0 x -2 2])