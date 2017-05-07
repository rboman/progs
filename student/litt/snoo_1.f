C      +----------------------+
C      !     LITT splines     !
C      +----------------------+
C                      13.03.95

C      Partie 1: Queue, corps
C

       integer n,I,m
       parameter (n=10)
       parameter (m=200)
       double precision x(m+1),y(m+1),XN(n),YN(n),DN(n),W(3*n),x1,y1

C      ------------------------
C               Data
C      ------------------------

       data YN/0.0 , 0.3 , 0.35 , 0.4 , 0.6 , 1.2 , 1.55, 1.5 ,
     *         1.1 , 0.6/
 
       data XN/0.0 , 0.5 ,1.9, 2.0 , 2.1 , 2.5 ,
     *         3.5 , 4.0 , 5.0 , 5.9/ 

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

       open(UNIT = 2, FILE = 'snoo1.m')
          do 100 I=1,m+1
             write(2,*)'x1(',I,')=',X(I),';'
             write(2,*)'y1(',I,')=',Y(I),';'
100       continue

          do 200 I=1,n
             write(2,*)'xn1(',I,')=',XX(I),';'
             write(2,*)'yn1(',I,')=',YY(I),';'
200       continue
       
          write(2,*)'plot(xn1,yn1,''w+'',x1,y1,''w-'')'
          write(2,*)'axis(''equal'');grid'
       close (UNIT = 2)

       return
       end
      


