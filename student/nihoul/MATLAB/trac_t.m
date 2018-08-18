
figure()
N=size(a);
j=0;
for i=1:2:N(1)/N(2)
   j=j+1;
   subplot(3,2,j)
   contour(a((i-1)*N(2)+1:i*N(2),:)','k')        
   title('t = 0.2')
end

