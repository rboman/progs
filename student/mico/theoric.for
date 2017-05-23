C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     Projet MICO - Partie theorique (ver. FORTRAN 21.03.95)
C
C            Calcule u(v) et P(u) par Newton Raphson
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C ----Declarations--------
      integer i, boucle, vz
      double precision h, a, b, Vh, Vv, E, Lh, Lv
      double precision TOL, u, lastu, F, DF, Du, Pl(16), ul(16)
      double precision P(101), x(101), y(101)

C ----Donnees-------------     
      h   = 40. 
      a   = 24. 
      b   = 40. 
      Vh  = 160. 
      Vv  = 200. 
      E   = 70000.
      Lh  = 2*a 
      Lv  = sqrt(h**2+(b+a)**2)
      TOL = 1e-5

C ----Trace u fct. de v---
      i=0 
      u=0. 
      lastu=0.

      do 10 v=0,90
         i=i+1
         boucle=0
         lastu=0
         F=(Vv/Lv**4)*(v**2+u**2-2*h*v+2*u*(a+b))*(u+a+b)
         F=F+(8*Vh/Lh**4)*(u**2+2*a*u)*(u+a)
         DF=(Vv/Lv**4)*((u+a+b)*(2*u+2*(a+b))
     *      +(v**2+u**2-2*h*v+2*u*(a+b)))
         DF=DF+(8*Vh/Lh**4)*((2*u+2*a)*(u+a)+(u**2+2*u*a))
         Du=-F/DF 
         u=u+Du

20       F=(Vv/Lv**4)*(v**2+u**2-2*h*v+2*u*(a+b))*(u+a+b)
         F=F+(8*Vh/Lh**4)*(u**2+2*a*u)*(u+a)
         if (abs(u-lastu).GT.TOL) then
            DF=(Vv/Lv**4)*((u+a+b)*(2*u+2*(a+b))
     *         +(v**2+u**2-2*h*v+2*u*(a+b)))
            DF=DF+(8*Vh/Lh**4)*((2*u+2*a)*(u+a)+(u**2+2*u*a))
            Du=-F/DF 
            lastu=u 
            u=u+Du
         else
            boucle=1
         endif
         if (boucle.EQ.0) goto 20

         x(i)=v
         y(i)=u
         P(i)=(E*Vv/Lv**4)*(v*v+u*u-2*h*v+2*u*(a+b))*(v-h)
10    continue

C ----R‚ponse lin‚aire----

      do 40 vz=0,15
        ul(vz+1)=2*h*vz*(a+b)*Vv/Lv**4/(Vh/a**2+2*Vv*(a+b)**2/Lv**4)
        Pl(vz+1)=-2*h*Vv*E/Lv**4*(ul(vz+1)*(a+b)-h*vz)
40    continue

C ----Resultats-----------  

32    open (UNIT = 1, FILE = 'th_1.m', STATUS='unknown')          
      do 31 i=1,91
         write(1,*)'xt(',i,')=',x(i),';'
         write(1,*)'yt(',i,')=',y(i),';'  
         write(1,*)'Pt(',i,')=',P(i),';'  
31    continue     
      do 30 i=1,16   
         write(1,*)'ul(',i,')=',ul(i),';'  
         write(1,*)'Pl(',i,')=',Pl(i),';'  
30    continue
      write(1,*)'figure(1), plot(xt,yt,''k''), grid'
      write(1,*)'figure(2), plot(xt,Pt,''k''), grid'
      write(1,*)'figure(3), plot(0:15,Pl,''k'',xt,Pt,''k''), grid'
      write(1,*)'figure(4), plot(xt,yt,''k'',0:15,ul,''k''), grid'
      close (UNIT=1)
      end
