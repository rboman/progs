C      +----------------------+
C      !     LITT splines     !
C      +----------------------+
C                      20.03.95

C      Snoopy (entier): une seule spline.
C                       trace les derivees


       integer n,I,J,m,K
       parameter (n=29)
       parameter (m=200)
       double precision x(m+1),y(m+1),XN(n),YN(n),DN(n),W(3*n),x1,y1
       double precision vals(4),deriv(m+1,4)
       
       common /TG02BD/ K
C      ------------------------
C               Data
C      ------------------------

       data YN/0.0, 0.3, 0.35, 0.4, 0.6, 1.2, 1.55, 1.5,
     *         1.1, 0.65, 0.6, 0.7, 1.45, 1.5, 1.5, 1.45, 1.5, 
     *         1.48, 1.44, 1.2, 1.1, 1.0, 0.8, 0.55, 0.4, 0.4,
     *         0.3, 0.2, 0.0/ 


       data XN/0.0 , 0.5 ,1.9, 2.0 , 2.1 , 2.5 ,
     *         3.5 , 4.0 , 5.0 , 5.8 , 5.9 , 
     *         6.0 , 7.15, 7.35, 7.5 , 7.6, 7.7, 7.8,
     *         7.9 , 8.15, 8.25, 8.5 , 9.5 ,
     *         10.0,10.25, 10.3, 10.7, 10.85, 11.0/

C      ------------------------
C           Interpolation
C      ------------------------
       call TB04AD(n,XN,YN,DN,W)
       
       x1=XN(1)
       do 10 I=1,m+1
          call TG02AD(1,n,XN,YN,DN,x1,vals)
          x(I)=x1
          do 20 J=1,4
             deriv(I,J)=vals(J)
20        continue
          x1=x1+(XN(n)-XN(1))/m
10     continue
    
       call tomat(n,m,XN,YN,x,deriv)

       end 

C      ------------------------
C        Graphique -> Matlab
C      ------------------------

       subroutine tomat(n,m,XX,YY,X,DER)

       integer m,n,I
       double precision X(m+1),DER(m+1,4),XX(n),YY(n)

       open(UNIT = 2, FILE = 'snoo_derive.m')
          do 100 I=1,m+1
             write(2,*)'x(',I,')=',X(I),';'
             write(2,*)'y(',I,')=',DER(I,1),';'
             write(2,*)'d1(',I,')=',DER(I,2),';'
             write(2,*)'d2(',I,')=',DER(I,3),';'
             write(2,*)'d3(',I,')=',DER(I,4),';'
100       continue

          do 200 I=1,n
             write(2,*)'xn(',I,')=',XX(I),';'
             write(2,*)'yn(',I,')=',YY(I),';'
200       continue
          write(2,*)'figure(1)'
          write(2,*)'plot(xn,yn,''w+'',x,y,''w-'')'
          write(2,*)'figure(2)'
          write(2,*)'plot(x,d1,''w-'');grid'
          write(2,*)'title(''Derivee premiere (snoopy entier)'')'
          write(2,*)'figure(3)'
          write(2,*)'plot(x,d2,''w-'');grid' 
          write(2,*)'title(''Derivee seconde (snoopy entier)'')'
          write(2,*)'figure(4)'
          write(2,*)'plot(x,d3,''w-'');grid'
          write(2,*)'title(''Derivee troisieme (snoopy entier)'')'
       close (UNIT = 2)

       return
       end
      


