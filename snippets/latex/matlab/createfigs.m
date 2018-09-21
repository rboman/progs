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

saveas(fig, 'figure1.eps', 'epsc')
%saveas(fig, 'figmatlab1.pdf')             % 1 full page
%print(gcf, '-dpdf', 'figmatlab1.pdf');    % 1 full page

warning('off', 'MATLAB:print:FigureTooLargeForPage'); % sometimes, the margins are too tight, but it is OK
fig.PaperPositionMode = 'auto'; % export your figures at the same size as they are on the screen
fig_pos = fig.PaperPosition;
fig.PaperSize = [fig_pos(3) fig_pos(4)]; % set papersize to figure size (otherwise it is a full page!)
print(fig, 'figure1', '-dpdf')
