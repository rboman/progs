close all; clear all;

%load Temp.txt
load ../Release/Temp_couplage.txt
Temp=Temp_couplage;

L0=45e-6;
dt=1e-9;
m=size(Temp,2);
nt=size(Temp,1)/2;

x=linspace(-L0/2,L0/2,m);
t=0:nt-1;
t=t*dt;

T=Temp(1:2:2*nt-1,:);
u=Temp(2:2:2*nt,:);
Tc=Temp_couplage(1:2:2*nt-1,:);
uc=Temp_couplage(2:2:2*nt,:);

figure(1)
mesh(x,t,T)
xlabel('x [m]')
ylabel('t [s]')
zlabel('temperature T [K]')

figure(2)
mesh(x,t,u)
xlabel('x [m]')
ylabel('t [s]')
zlabel('displacement u [mm]')

figure(3)
plot(t,T(:,(m-1)/2))
xlabel('t [s]')
ylabel('T(x=0) [K]')
grid

figure(4)
plot(x,T(end,:))
xlabel('x [m]')
ylabel('T(x=0) [K]')
grid