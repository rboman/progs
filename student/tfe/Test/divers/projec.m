A=[[1 1];[1 3]];
b=[1 1];
i1=0;
for i=-1:0.1:2
   i2=0;
   i1=i1+1;
   for j=-1:0.1:2
   i2=i2+1;
   x=[i j];
   f(i1,i2)=x*A*x'-x*b';
   end
end
vx1=[0 10];
vy1=[10 10];

contour(f),hold on
plot(vx1,vy1)
