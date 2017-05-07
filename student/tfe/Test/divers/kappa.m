K=10;
i=0;
for N=0:10:1000
   i=i+1;
   j=0;
   for K=5:5:30
      j=j+1;
      y(i,j)=sqrt((K^2-1)/K^2)^N;
   end
end
plot(0:10:1000,y)
   