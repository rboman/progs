console_d.exe >out.txt

diff -q -s mydomain.dat verif/mydomain.dat.orig
diff -q -s mydomain.e verif/mydomain.e.orig
diff -q -s mydomain.m verif/mydomain.m.orig
diff -q -s mymatrix.dat verif/mymatrix.dat.orig
diff -q -s mymatrix.don verif/mymatrix.don.orig
diff -q -s mymatrix.e verif/mymatrix.e.orig
diff -q -s mymatrix.m verif/mymatrix.m.orig
diff -q -s out.txt verif/out.txt.orig
