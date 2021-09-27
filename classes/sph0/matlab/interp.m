
function interp()
    close all; clear all;
    
    % plot M4 spline kernel
    x=linspace(-3,3,100);
    figure;
    h=0.5;
    plot(x,m4spline(x,h));
    hold on
    plot(x,gausskernel(x,h));
    grid;
    xlabel('x')
    ylabel('kernel')
    legend('M4 spline', 'gaussian')
    
    % verify that int W = 1
    %integral(@(x)m4spline(x,h),-3*h,3*h)
    integral(@(x)gausskernel(x,h),-Inf,Inf)
    
    % plot a sin and the approximate fct
    x=linspace(1,10,200);
    y=myfun(x);
    
    figure;
    plot(x,y);
    xlabel('x')
    grid;  
    hold on;
    h=2;
    plot(x,approx(x,h))
    h=0.5;
    plot(x,approx(x,h))
    legend('exact','h=2','h=0.5');
    
end

function y=myfun(x)
    %y=(x-5).^2/10;
    y=sin(x);
end


function v=m4spline(x,h)
    r=abs(x)/h;
    i1=find(r<1);
    i2=find(r>=1 & r<2);
    v=zeros(size(r));
    v(i1) = (((2-r(i1)).^3)-4*((1-r(i1)).^3))/6;
    v(i2) = ((2-r(i2)).^3)/6;
    v=v/h;
end

function v=gausskernel(x,h)
    v = exp(-(x.*x)/(h^2))/(pi^(1/2)*h); % 1D
end


function v=approx(x,h)
    f = @(xp,c) m4spline(c-xp, h).*myfun(xp);
    %f = @(xp,c) gausskernel(c-xp, h).*myfun(xp);
    
    
    for i=1:length(x)
        v(i) = integral(@(xp)f(xp,x(i)),x(i)-3*h,x(i)+3*h);
    end
end



   
   