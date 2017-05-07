C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C      Projet MICO - Newton Raphson (ver. FORTRAN 21.03.95) 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C ----Declarations--------
      integer i, boucle, ii
      double precision h, a, b, Vh, Vv, E, Lh, Lv, compt
      double precision TOL, u
      double precision lambda
      double precision P(101), x(101), y(101), R(2), uu(2)
      double precision norm, F(2), Kt(2,2), Du(2), MAT(2)
      double precision invKt(2,2), dtmKt

C ----Donnees-------------     
      h      = 40. 
      a      = 24. 
      b      = 40. 
      Vh     = 160. 
      Vv     = 200. 
      E      = 70000.
      Lh     = 2*a 
      Lv     = sqrt(h**2+(b+a)**2)
      TOL    = 1e-5
      i      = 0
      lambda = 0.
      uu(1)  = 0.
      uu(2)  = 0.
      R(1)   = 9000.
      R(2)   = 0.
      
C ----Boucle principale----

      do 10 compt=0,100
         i=i+1
         lambda=compt/100.
         boucle=0
         ii=0
20       u=uu(1) 
         v=uu(2)
         ii=ii+1
         if (ii.GT.200) then     
            boucle=1
         endif
         F(1)=(E*Vv/Lv**4)*(v*v+u*u-2*h*v+2*u*(a+b))*(v-h)
         F(2)=(Vv/Lv**4)*(v**2+u**2-2*h*v+2*u*(a+b))*(u+a+b)
         F(2)=F(2)+(8*Vh/Lh**4)*(u**2+2*a*u)*(u+a)
         if (lambda.NE.0) then
            norm=dsqrt((lambda*R(1)-F(1))**2+(lambda*R(2)-F(2))**2)
     *           /abs(lambda)/dsqrt(R(1)**2+R(2)**2)
C         write(*,*)norm
         else
            norm=0
         endif
         if (norm.GT.TOL) then
            Kt(1,1)=E*Vv/(Lv**4)*(2*u+2*(a+b))*(v-h)
            Kt(1,2)=E*Vv/(Lv**4)*((v**2+u**2-2*v*h+2*u*(a+b))
     *              +(v-h)*(2*v-2*h))
            Kt(2,1)=Vv/(Lv**4)*((v**2+u**2-2*h*v+2*u*(a+b))
     *              +(u+a+b)*(2*u+2*(a+b)))+8*Vh/(Lh**4)*((2*u+2*a)
     *              *(u+a)+(u**2+2*a*u))
            Kt(2,2)=Vv/(Lv**4)*(u+a+b)*(2*v-2*h)
            MAT(1)=lambda*R(1)-F(1)
            MAT(2)=lambda*R(2)-F(2)
            dtmKt=Kt(1,1)*Kt(2,2)-Kt(1,2)*Kt(2,1)
            invKt(1,1)=Kt(2,2)/dtmKt
            invKt(2,2)=Kt(1,1)/dtmKt
            invKt(1,2)=-1*Kt(1,2)/dtmKt
            invKt(2,1)=-1*Kt(2,1)/dtmKt
            Du(1)=invKt(1,1)*MAT(1)+invKt(1,2)*MAT(2)
            Du(2)=invKt(2,1)*MAT(1)+invKt(2,2)*MAT(2)  
            uu(1)=uu(1)+Du(1)
            uu(2)=uu(2)+Du(2)      
         else
            boucle=1
         endif
         if (boucle.EQ.0) goto 20
         x(i)=uu(2)
         y(i)=uu(1)
         P(i)=lambda*R(1)
10    continue

C ----Resultats-----------  

32    open (UNIT = 1, FILE = 'nr_1.m', STATUS='unknown')
40    do 30 i=1,101
         write(1,*)'x(',i,')=',x(i),';'
         write(1,*)'y(',i,')=',y(i),';'  
         write(1,*)'P(',i,')=',P(i),';'  
30    continue
      write(1,*)'figure(1), plot(x,y), grid'
      write(1,*)'figure(2), plot(x,P), grid'
      close (UNIT=1)
      end
