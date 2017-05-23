N=size(a)
figure(1)
clabel(contour(a((i-1)*N(2)+1:i*N(2),:)','k')) 
xlabel('i'), ylabel('j')       
hold on
quiver(1:2:N(2),1:2:N(2),u((i-1)*N(2)+1:2:i*N(2),1:2:N(2))',v((i-1)*N(2)+1:2:i*N(2),1:2:N(2))',1,'k')
hold off

figure(2)
mesh(a((i-1)*N(2)+1:i*N(2),:)')
colormap([0 0 0])
xlabel('i'), ylabel('j')

