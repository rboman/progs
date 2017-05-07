
c1 = [2, 6];
c2 = [-2, 5];

R1 = 5;
R2 = 1.5;

th = 0:0.1:2*pi;
th_d = th*180/pi;

f = (c2(1)-c1(1))*cos(th) + (c2(2)-c1(2))*sin(th) - R1+R2;

sig1=sign(f(size(f)));
nsol=0;
sol=0;
for i=1:size(f'),
    sig2 = sign(f(i));
    if sig2~=sig1
        nsol=nsol+1;
        sol(nsol) =th(i);
        disp(sprintf('approx solution #%d : theta=%f degres',i,th(i)*180/pi))
    end
    sig1 = sig2;
end

% solve
for n = 1:nsol,
    f_x=1;
    x=sol(n);
    i=0;
    while abs(f_x)>1e-6,
        f_x = (c2(1)-c1(1))*cos(x) + (c2(2)-c1(2))*sin(x) - R1+R2;
        disp(sprintf('iteration %d : residu=%f',i,f_x))
        df_x = -(c2(1)-c1(1))*sin(x) + (c2(2)-c1(2))*cos(x);
        dx = -f_x/df_x;
        x=x+dx;
        i=i+1;
    end
    theta(n)=x;
    disp(sprintf('solution #%d : theta=%f degres',n,x*180/pi))
end

% trace la fonction
figure(1)
plot(th_d, f)
grid

% trace les cercles
figure(2)
circ1 = c1'*ones(size(th))+R1*[cos(th); sin(th)];
circ2 = c2'*ones(size(th))+R2*[cos(th); sin(th)];
plot(circ1(1,:), circ1(2,:), circ2(1,:), circ2(2,:) );
hold on
for i= 1:nsol,
    x = theta(i);
    p1 = c1' + R1*  [cos(x); sin(x)];
    p2 = c2' + R2*  [cos(x); sin(x)];
    s=0:0.1:1;
    tg = p1*s+p2*(1-s);
    plot( tg(1,:), tg(2,:) );
end
hold off
axis equal
grid



