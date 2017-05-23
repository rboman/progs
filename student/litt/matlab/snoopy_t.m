%        ----------------------------------------------------------
%              Trace Snoopy grace a 3 interpolations spline
%        ----------------------------------------------------------
%                                                          14.03.95


% Calcule les 3 parties separement

snoo1
snoo2
snoo3


% Colle les vecteurs xi et yi

xx=[x1,x2,x3];
yy=[y1,y2,y3];
xxn=[xn1,xn2,xn3];
yyn=[yn1,yn2,yn3];


% Trace le resultat (3x200 pts)
figure(3)
plot(xx,yy,'k-',xxn,yyn,'k+')
axis('equal')
title('Snoopy (3 splines)')
grid
