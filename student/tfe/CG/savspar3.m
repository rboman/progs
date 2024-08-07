%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%            Cr�ation d'une matrice creuse 
%            et sauvegarde dans SYSTEM2.BIN
%
% Update : 12.12.96 pour GMRES4.FOR
%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

mak1=0;                 % mak1=0 : pas de SYSTEM.BIN
                        %           (full matrix).
ilu0=1;                 % ilu0!=0 : �l�m. diag non nuls.

N      = 100;           % dimension du syst.
COND   = 0.001;          % inverse de cond(A).
DENSITY= 0.3;           % densit� approx.

% Cr�ation :
% --------

A=sprandsym(N,DENSITY,COND,1); b=rand(1,N);

%spy(A)
P=symrcm(A);           % R�org. de la matrice.
A=A(P,P);

if ilu0~=0
   for i=1:N
      A(i,i)=0.5; %*rand(1);  % /1e5+A(i,i);
   end
end
disp('Cr�ation termin�e')

% Conversion format MATLAB -> format CSR :
% --------------------------------------

NELEM=0;
IS(1)=1;
S=[];
for i=1:N
   for j=find(A(i,:))
      NELEM=NELEM+1;
      JS(NELEM)=j;        
      val = 0+A(i,j);
      S=[S val];
   end
   IS(i+1)=NELEM+1;
end

% Sauvegarde dans SYSTEMx.BIN :
% ---------------------------

disp('SYSTEM2.BIN')
fid = fopen('system2.bin','wt');

fprintf(fid, '%d\n',N);    		% -- Save size -----
fprintf(fid, '%d\n',NELEM);

for i=1:NELEM                  		% -- Save S et JS --
   fprintf(fid, '%18.16f\n',S(i));
   fprintf(fid, '%d\n',JS(i));
end
for i=1:N+1                  		% -- Save IS -------
   fprintf(fid, '%d\n',IS(i));  
end
for i=1:N                 		% -- Save b --------
   fprintf(fid, '%18.16f\n',b(i));  
end
fclose(fid);


if (mak1~=0)
   disp('SYSTEM.BIN')

   fid = fopen('system.bin','wt');

   fprintf(fid, '%d\n',N);    		% -- Save size --

   for j=1:N                		% -- Save A -----
      for i=1:N
         fprintf(fid, '%18.16f\n',A(i,j));
      end
   end

   for i=1:N				% -- Save b -----
      fprintf(fid, '%18.16f\n',b(i));
   end
   fclose(fid);
end

% Sauvegarde pour MATLAB :
% ----------------------
save a.mat A b
disp('Done..') 