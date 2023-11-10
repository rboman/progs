% display the kernels and smoothed approximations of a given function

function interp()
    close all;
    
    % plot M4 spline kernel
    x = linspace(-2, 2, 100);
    openfig();
    h = 0.5;
    plot(x, m4spline(x, h), 'LineWidth', 2);
    plot(x, gausskernel(x, h), 'LineWidth', 2);
    xlabel('x')
    ylabel('kernel')
    legend('M4 spline', 'gaussian')
    title('Comparison of 2 SPH kernels')  
    %saveas(gcf,'fig_kernels.png');
    exportgraphics(gcf,'fig_kernels.png','Resolution',100)

    % verify that int W = 1
    %integral(@(x)m4spline(x,h), -3*h, 3*h)
    integral(@(x)gausskernel(x,h), -Inf, Inf)
    
    % plot a sin and the approximate fct
    x = linspace(1, 10, 200);
    y = myfun(x);
    
    openfig();
    plot(x, y, 'LineWidth', 2);
    xlabel('x')
    ylabel('f(x)')
    h = 2;
    plot(x, approx(x, h), 'LineWidth', 2)
    h = 0.5;
    plot(x, approx(x, h), 'LineWidth', 2)
    legend('exact', 'h = 2', 'h = 0.5');
    title('A function and its smoothed approximations')
    yl = ylim; dy = yl(2)-yl(1);
    ylim([yl(2)-dy*1.1 yl(1)+dy*1.4]);
    xlim([1 10]);
    %saveas(gcf,'fig_smooth_sin.png');
    exportgraphics(gcf,'fig_smooth_sin.png','Resolution',100)

end

function h = openfig()
    h = figure;
    ax = gca;
    ax.GridAlpha = 0.3;
    ax.FontSize = 12;
    box on; grid on; hold on; 
end

function y = myfun(x)
    %y = (x-5).^2/10;
    y = sin(x);
end

function v = m4spline(x,h)
    r = abs(x)/h;
    i1 = find(r<1);
    i2 = find(r>=1 & r<2);
    v =  zeros(size(r));
    v(i1) = (((2-r(i1)).^3)-4*((1-r(i1)).^3))/6;
    v(i2) = ((2-r(i2)).^3)/6;
    v = v/h;
end

function v = gausskernel(x, h)
    v = exp(-(x.*x)/(h^2))/(pi^(1/2)*h); % 1D
end

function v = approx(x, h)
    f = @(xp,c) m4spline(c-xp, h).*myfun(xp);
    %f = @(xp,c) gausskernel(c-xp, h).*myfun(xp);
    
    v = zeros('like', x);
    for i = 1:length(x)
        v(i) = integral(@(xp)f(xp,x(i)), x(i)-3*h, x(i)+3*h);
    end
end
 
   