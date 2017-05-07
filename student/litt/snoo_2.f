C      +----------------------+
C      !     LITT splines     !
C      +----------------------+
C                      13.03.94

C      Partie 2: Oreille
C

       integer n,I,m
       parameter (n=4)
       parameter (m=200)
       double precision x(m+1),y(m+1),XN(n),YN(n),DN(n),W(3*n),x1,y1

C      ------------------------
C               Data
C      ------------------------

       data YN/0.6, 0.7, 1.5, 1.5 , 1.45/
 
       data XN/5.9, 6.0, 7.35 , 7.5 , 7.6/

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

       open(UNIT = 2, FILE = 'snoo2.m')
          do 100 I=1,m+1
             write(2,*)'x2(',I,')=',X(I),';'
             write(2,*)'y2(',I,')=',Y(I),';'
100       continue

          do 200 I=1,n
             write(2,*)'xn2(',I,')=',XX(I),';'
             write(2,*)'yn2(',I,')=',YY(I),';'
200       continue
       
          write(2,*)'plot(xn2,yn2,''w+'',x2,y2,''w-'')'
          write(2,*)'axis(''equal'');grid'
       close (UNIT = 2)

       return
       end
      


