C      +----------------------+
C      !     LITT splines     !
C      +----------------------+
C                      14.03.95

C      Partie 3: Tete, nez, patte
C

       integer n,I,m
       parameter (n=14)
       parameter (m=200)
       double precision x(m+1),y(m+1),XN(n),YN(n),DN(n),W(3*n),x1,y1

C      ------------------------
C               Data
C      ------------------------

       data YN/1.45 , 1.5 , 1.48 , 1.44 ,
     *         1.2, 1.1 , 1.0 ,
     *         0.8, 0.55, 0.4, 0.4, 0.3 , 0.2 , 0.0/ 

       data XN/ 7.6, 7.7, 7.8,
     *         7.9 , 8.15, 8.25, 8.5 , 9.5 ,
     *         10.0,10.25, 10.3, 10.7, 10.85, 11.0/

C      ------------------------
C        Interpolation spline
C      ------------------------
       call TB04AD(n,XN,YN,DN,W)
       
       x1=XN(1)
       do 10 I=1,m+1
          y1=TG01BD(1,n,XN,YN,DN,x1)
          x(I)=x1
          y(I)=y1
          x1=x1+(XN(n)-XN(1))/m
10     continue
    
       call tomat(n,m,XN,YN,x,y)

       end 

C      ------------------------
C        Graphique -> Matlab
C      ------------------------

       subroutine tomat(n,m,XX,YY,X,Y)

       integer m,n,I
       double precision X(m),Y(m),XX(n),YY(n)

       open(UNIT = 2, FILE = 'snoo3.m')
          do 100 I=1,m+1
             write(2,*)'x3(',I,')=',X(I),';'
             write(2,*)'y3(',I,')=',Y(I),';'
100       continue

          do 200 I=1,n
             write(2,*)'xn3(',I,')=',XX(I),';'
             write(2,*)'yn3(',I,')=',YY(I),';'
200       continue
       
          write(2,*)'plot(xn3,yn3,''w+'',x3,y3,''w-'')'
          write(2,*)'axis(''equal'');grid'
       close (UNIT = 2)

       return
       end
      


