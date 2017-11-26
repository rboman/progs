% figures matlab "propres"
clear all; close all;

% generate some data
scaling = 3.3e-5;
x = linspace(-3*pi,3*pi,100);
y1 = sin(x) * scaling;
y2 = (sin(2*x)+cos(x))/2 * scaling;

% figure 1 - default
fig = figure;
hold on; 
%grid on; box on;
%fig.Position = [100 100 560*0.57 420*0.57];
%ax = gca;
%ax.GridAlpha=0.3;
%ax.FontSize=8;
plot(x*scaling,y1);
plot(x*scaling,y2);
xlabel('xlabel')
ylabel('ylabel')

saveas(fig, 'figmatlab1.eps', 'epsc')
%saveas(fig, 'figmatlab1.pdf') % 1 page complete
%print(gcf, '-dpdf', 'figmatlab1.pdf');

fig.PaperPositionMode = 'auto';
fig_pos = fig.PaperPosition;
fig.PaperSize = [fig_pos(3) fig_pos(4)];
print(fig,'figmatlab1','-dpdf')