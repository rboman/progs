%             ----------------------------------------------
%             Calcul de la vitesse moy. d'avancement du film
%             ----------------------------------------------
%

som=0;                    
thetamin=6.2696;
thetamax=3.1448;
pas=5*pi/180;
vitmax=0;

a=floor(thetamin/pas)+1;
b=floor(thetamax/pas)+1;
if b>a+1,
   for i=(a+1):(b-1)
      som=som+xpp(i)*pas;   
      if xpp(i)<vitmax, vitmax=xpp(i);, end
   end 
   som=som+(xpp(a)+xpp(b))*pas/2;
   interval=(b-a)*pas;  
end
if (b-a)==1,
   som=(xpp(a)+xpp(b))*pas/2;
   interval=(b-a)*pas;  
end
if a>b,
   for i=(a+1):73,
      som=som+xpp(i)*pas;
      if xpp(i)<vitmax, vitmax=xpp(i);, end
   end
   for i=2:(b-1),
      som=som+xpp(i)*pas;
      if xpp(i)<vitmax, vitmax=xpp(i);, end
   end
   som=som+(xpp(a)+xpp(b))*pas/2;
   interval=2*pi-(a-b)*pas;  
end
if xpp(a)<vitmax, vitmax=xpp(a);, end
if xpp(b)<vitmax, vitmax=xpp(b);, end

vit_moy=-som/(2*pi)
vit_moy2=-som/(interval)
vit_max=-vitmax

