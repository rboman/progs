close all; clear all;

testname = 'test3';

%load Temp.txt
run([ testname '/xbar.m']);
run([ testname '/xmsh.m']);
run([ testname '/xlight.m']);
run([ testname '/xnmark.m']);

load([ testname '/time.txt']);
load([ testname '/disp.txt']);
load([ testname '/temp.txt']);
load([ testname '/source.txt']);

x=linspace(-bar.L/2,bar.L/2,msh.m);

Tp=light.Q/2/bar.k;
T0a=bar.T0+Tp*bar.L/2;
ua = -sign(x-1e-16)*bar.alpha*light.Q/4/bar.k.*x.*(x-sign(x-1e-16)*bar.L/2);
epsa = -sign(x-1e-16)*bar.alpha*light.Q/4/bar.k.*(2*x-sign(x-1e-16)*bar.L/2);

figure(1)
mesh(x,time,temp)
xlabel('x [m]')
ylabel('t [s]')
zlabel('temperature T [K]')

figure(2)
mesh(x,time,disp)
xlabel('x [m]')
ylabel('t [s]')
zlabel('displacement u [mm]')

figure(3)
hold on
title('Evolution de la temperature au milieu du barreau')
plot(time,temp(:,(msh.m+1)/2))
plot(time,T0a*ones(size(time)),'r')
xlabel('t [s]')
ylabel('T(x=0) [K]')
grid

figure(4)
hold on
title('Champ final de température')
plot(x,temp(end,:))
plot(0, T0a,'rx','LineWidth', 3);
xlabel('x [m]')
ylabel('T(x=0) [K]')
grid

figure(5)
plot(time,disp(:,floor((msh.m-1)/4)))
xlabel('t [s]')
ylabel('u [m]')
grid

figure(6)
title('Champ de déplacement final')
hold on
plot(x,disp(end,:))
plot(x,ua,'r--')
xlabel('x [m]')
ylabel('u [m]')
grid

dx=bar.L/(msh.m-1);

umax = bar.alpha*light.Q*bar.L^2/64/bar.k;

figure(7)
title('Champ de défo')
hold on
plot(x(2:end-1),(disp(end,3:end)-disp(end,1:end-2))/2/dx)
plot(x,epsa,'r')
xlabel('x [m]')
ylabel('eps [m]')
grid

l = sqrt(bar.E/bar.rho);
dtmax=dx/l;


