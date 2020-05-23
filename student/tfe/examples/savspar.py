#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#            Création d'une matrice creuse
#            et sauvegarde dans SYSTEM2.BIN
#
# Update : 31.01.96 pour SYMMLQ.FOR
#          !!!!!!!OK 2019!!!!!!
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++

import matplotlib.pyplot as plt
import scipy.sparse as sparse
import scipy.stats as stats
import numpy as np
mak1 = 0                 # mak1=0 : pas de SYSTEM.BIN
#           (full matrix).
ilu0 = False                 # ilu0~=0 : élém. diag non nuls.
isym = False                # isym=1 : matrice symetrique
N = 100           # dimension du syst.
COND = 0.001         # inverse de cond(A).
DENSITY = 0.1           # densité approx.

#np.random.seed((3, 14159))  # resultats prévisibles


def sprandsym(n, density):
    rvs = stats.norm().rvs
    X = sparse.random(n, n, format='csr', density=density, data_rvs=rvs)
    upper_X = sparse.triu(X)
    result = upper_X + upper_X.T - sparse.diags(X.diagonal())
    return result


def sprand(n, density):
    rvs = stats.norm().rvs
    X = sparse.random(n, n, format='csr', density=density, data_rvs=rvs)
    return X


print("generating random matrix...")
if isym:
    A = sprandsym(N, DENSITY)
else:
    A = sprand(N, DENSITY)

print(repr(A))
print("calculating cond number...")
norm_A = sparse.linalg.norm(A)
norm_invA = sparse.linalg.norm(sparse.linalg.inv(A))
cond = norm_A*norm_invA
print("cond(A)=", cond)
#print "nnz=", A.nnz
#print "A.data", A.data
#print "A.indices", A.indices
#print "A.indptr", A.indptr

print("generating random rhs...")
b = np.random.rand(N)
#print "b=", b

print("solving with scipy...")
x = sparse.linalg.spsolve(A, b)
print("x=", x)
res = np.linalg.norm(b-A.dot(x))
print("=> residual=", res)

print("saving matrix to system2.bin...")
f = open('system2.bin', 'wt')
f.write('%d\n' % N)
f.write('%d\n' % A.nnz)
for i in range(len(A.data)):
    f.write('%18.16f\n' % A.data[i])
    f.write('%d\n' % (A.indices[i]+1))
for i in range(len(A.indptr)):
    f.write('%d\n' % (A.indptr[i]+1))
for v in b:
    f.write('%18.16f\n' % v)
for v in x:
    f.write('%18.16f\n' % v)
f.close()
print('done.')


"""
P=symrcm(A);           % Réorg. de la matrice.
A=A(P,P);
N=size(A,1);

if ilu0~=0
   for i=1:N
      A(i,i)=rand(1)/10;  % /1e5+A(i,i);
   end
end

disp('Création terminée')

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

"""
