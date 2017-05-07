C      +----------------------+
C      !     LITT splines     !
C      !    point anguleux    !
C      +----------------------+
C                      06.03.95


       integer n,m,I
       parameter (n=7)
       parameter (m=100)
       double precision x(m),y(m),Y1(m),XN(n),YN(n),DN(n),W(3*n)
       double precision vals(4), dy(m), ERREUR, h

C      ------------------------
C               Data
C      ------------------------

       DATA YN/1.0, 0.8, 0.3, 0.0, 0.3, 0.8, 1.0/

       DATA XN/0.0, 0.2, 0.7, 1.0, 1.3, 1.8, 2.0/


C      ------------------------
C               SPLINE
C      ------------------------

       call TB04AD(n,XN,YN,DN,W)       
 
C      ------------------------
C        Interpolation SPLINE
C      ------------------------

       x(1)=0.0
       y(1)=YN(1)
       y1(1)=YN(1)

       do 10 I=2,m
          x(I)  = 2.*I/m
          CALL  TG02AD(1,n,XN,YN,DN,x(I),vals)
          y(I)  = vals(1)
          dy(I) = vals(2)
          if(I.lt.m/2) then
             Y1(I)=-x(I)+1
          else
             Y1(I)=x(I)-1
          endif
10     continue

       h=2./m
       ERREUR=0
       do 5 I=1,m-1
          ERREUR=ERREUR+(abs(y(I)-Y1(I))+abs(y(I+1)-Y1(I+1)))*h/2
5      continue

       call tomat(n,m,XN,YN,x,y,Y1,dy,ERREUR)

       end

C      ------------------------
C             -> Matlab
C      ------------------------

       subroutine tomat(n,m,XX,YY,X,Y,Y1,DY,ERREUR)

       integer m,n,I
       double precision X(m),Y(m),Y1(m),XX(n),YY(n),DY(m),ERREUR

       open(UNIT = 2, FILE = 'ptang7e.m')

          do 100 I=1,m
             write(2,*)'x(',I,')=',X(I),';'
             write(2,*)'y(',I,')=',Y(I),';'
             write(2,*)'y1(',I,')=',Y1(I),';'
             write(2,*)'dy(',I,')=',DY(I),';'
100       continue

          do 200 I=1,n
             write(2,*)'xn(',I,')=',XX(I),';'
             write(2,*)'yn(',I,')=',YY(I),';'
200       continue
          write(2,*)'erreur=',ERREUR
          write(2,*)'figure(1);'
          write(2,*)'plot(xn,yn,''w+'',x,y,''w-'',x,y1,''w-'')'
          write(2,*)'axis(''equal'');grid;figure(2)'
          write(2,*)'plot(x,abs(y1-y),''w-'')'
          write(2,*)'grid;figure(3);'
          write(2,*)'plot(x,dy,''w-'');axis(''equal'');grid'
          write(2,*)'err(',n,')=max(abs(y1-y));'
          

       close (UNIT = 2)

       return

       end
       




