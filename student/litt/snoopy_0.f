C      +----------------------+
C      !     LITT splines     !
C      +----------------------+
C                      06.03.95

C      Snoopy (entier): interpolation avec 26 pts egalement espaces
C
C      Rem: Ce programme ne contient pasde routine pour le transfert
C           de donnees vers Matlab. Apres compilation, taper:
C                a.out >resultat.m

       integer n,I
       parameter (n=26)
       double precision x,s,XN(n),YN(n),DN(n),W(3*n)

C      ------------------------
C               Data
C      ------------------------
       data YN/0.0 , 0.3 , 0.35, 0.35, 0.35, 0.4, 0.6, 
     *         1.2 , 1.45, 1.55, 1.5 ,
     *         1.35, 1.1 , 0.85, 0.6 , 0.7 , 1.2, 1.4, 1.5 , 1.35, 1.0,
     *         0.8 , 0.75, 0.55, 0.3 , 0.0/ 


       data XN/0.0 , 0.5 , 1.0 , 1.5, 1.8, 2.0, 2.1, 2.5, 3.0,
     *         3.5 , 4.0 , 4.5 , 5.0, 5.5, 5.9, 
     *         6.0 , 6.5 , 7.0 , 7.5, 8.0, 8.5, 9.0, 9.5,
     *         10.0, 10.5, 11.0/

       do 30 I=1,n
          write(*,*)'xn(',I,')=',XN(I),';'
          write(*,*)'yn(',I,')=',YN(I),';'
30     continue

C      ------------------------
C        Interpolation spline
C      ------------------------
       call TB04AD(n,XN,YN,DN,W)       
 

C      ------------------------
C        Graphique -> Matlab
C      ------------------------
       x=0.0
       do 10 I=1,201
          s=TG01BD(1,n,XN,YN,DN,x)
          write(*,*)'x(',I,')=',x,'; y(',I,')=',s,';'
          x=x+11.0/200.0
10     continue

       end








