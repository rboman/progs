clear all; close all;

load area.txt
plot(area(:,1), area(:,2),'o-')
grid
xlabel('y')
ylabel('area')
