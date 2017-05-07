%===========================================================
%         Travail N.D.H. : Eléments aux frontières
%
% 1° partie : résolution du cercle.                22.11.96
%===========================================================

% Données numériques :
% --------------------

beta = 80;
k    = 400;
R    = 1.2;
a    = 1.2;

N     = 20;      % nbre d'éléments frontières
istep = 20;      % nbre de pas d'intégration

alpha=0:(2*pi/N):2*pi;
step=(2*pi/N/istep);

xf=R*cos(alpha);
yf=R*sin(alpha);

for i=1:N
   xel(i)=(xf(i)+xf(i+1))/2;
   yel(i)=(yf(i)+yf(i+1))/2;
end
plot(xf,yf)
hold on, plot(xel,yel,'*')
axis('square')

% Calcul des matrices G & H :
% ---------------------------
disp('Calcul des flux')

for i=1:N   
   for j=1:N
      if (j==i)
         dx=xf(i+1)-xf(i);
         dy=yf(i+1)-yf(i);
         dL=sqrt(dx*dx+dy*dy);
         G(i,i)=dL/(2*pi)*(log(2/dL)+1);
         H(i,i)=0.5;
      else
         G(i,j)=0.0;
         H(i,j)=0.0;

         for t=0:istep         
            xint(t+1)=xf(j)+((xf(j+1)-xf(j))/istep)*t;
            yint(t+1)=yf(j)+((yf(j+1)-yf(j))/istep)*t;
         end

%         yint=yf(j):((yf(j+1)-yf(j))/istep):yf(j+1);   foire

         nx=yf(j+1)-yf(j);
         ny=xf(j)-xf(j+1);
         temp=sqrt(nx*nx+ny*ny);
         nx=nx/temp;
         ny=ny/temp;
       
         for t=1:istep+1  
            temp=sqrt((xint(t)-xel(i))^2+(yint(t)-yel(i))^2);
            fct(t)=(log(1/temp)/2/pi);
            fct2(t)=(-nx*(xint(t)-xel(i))-ny*(yint(t)-yel(i)))/(2*pi*temp*temp);
         end
        
         for t=1:istep
%                    Calcul de G :
            
            dx=xint(t+1)-xint(t);
            dy=yint(t+1)-yint(t);
            dL=sqrt(dx*dx+dy*dy);

            G(i,j)=G(i,j)+(fct(t)+fct(t+1))/2*dL;

%                    Calcul de H :
            H(i,j)=H(i,j)+(fct2(t)+fct2(t+1))/2*dL;
         end
      end
   end
end
disp('Résolution du système')
pause

% Résolution de Hu = Gq (on cherche q) :
% --------------------------------------

for i=1:N
   u(i)=-beta/(2*k)*(xel(i)^2+yel(i)^2);
end

q=G\(H*u')


% Calcul de la solution à l'intérieur du domaine :
% ------------------------------------------------
disp('Calcul des températures')
pause

theta_range=0:pi/3:2*pi;
ray_range=0:R/5:R;
xbar=(ray_range'*cos(theta_range))';
ybar=(ray_range'*sin(theta_range))';
G=0.0; H=0.0;
i1=0;
for theta=theta_range
   i1=i1+1
   j1=0;
   for ray=ray_range
      j1=j1+1;
      for j=1:N
         G(j)=0.0;
         H(j)=0.0;
         for t=0:istep    
            xint(t+1)=xf(j)+((xf(j+1)-xf(j))/istep)*t;
            yint(t+1)=yf(j)+((yf(j+1)-yf(j))/istep)*t;        
         end
         
         nx=yf(j+1)-yf(j);
         ny=xf(j)-xf(j+1);
         temp=sqrt(nx*nx+ny*ny);
         nx=nx/temp;
         ny=ny/temp;

         for t=1:istep+1  
            temp=sqrt((xint(t)-xbar(i1,j1))^2+(yint(t)-ybar(i1,j1))^2);
            fct(t)=(log(1/temp)/2/pi);
            fct2(t)=(-nx*(xint(t)-xbar(i1,j1))-ny*(yint(t)-ybar(i1,j1)))/(2*pi*temp*temp);
         end
        
         for t=1:istep
%                    Calcul de G :
            
            dx=xint(t+1)-xint(t);
            dy=yint(t+1)-yint(t);
            dL=sqrt(dx*dx+dy*dy);

            G(j)=G(j)+(fct(t)+fct(t+1))/2*dL;

%                    Calcul de H :
            H(j)=H(j)+(fct2(t)+fct2(t+1))/2*dL;
         end
      end
      T(i1,j1)=G*q-H*u';
      r=sqrt(xbar(i1,j1)^2+ybar(i1,j1)^2);
      T(i1,j1)=T(i1,j1)+beta/(2*k)*r*r;      
      Texact(i1,j1)=beta/(2*k)*(r*r-R*R);
   end
end

plot(xbar,ybar,'bo'), hold off