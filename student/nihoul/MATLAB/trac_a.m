N=size(a)

M = moviein(N(2)/N(1));

for i=1:N(1)/N(2)
   figure(1)
   contour(a((i-1)*N(2)+1:i*N(2),:)')      
   M(:,i) = getframe;
end

movie(M,100)
