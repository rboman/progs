C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     Projet MICO - Riks & N.R.      (ver. FORTRAN 31.03.95)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


C ----Declarations--------
      integer i, boucle, t, ii, type
      double precision h, a, b, Vh, Vv, E, Lh, Lv, pas
      double precision TOL, u, Force(1000), Du1(2), TgSwap
      double precision lambda, Beta, ETA, Du(2), Du2(2)
      double precision x(1000), y(1000), R(2), uu(2)
      double precision norm, F(2), Kt(2,2), MAT(2)
      double precision invKt(2,2), dtmKt, uut(2), lambdat, Fint(2)
      double precision rac1, rac2, a1, a2, a3, test1, test2
      double precision ps, ru(2), ru2(2)

C ----Donnees-------------     
      h      = 40. 
      a      = 24. 
      b      = 40. 
      Vh     = 160. 
      Vv     = 200. 
      E      = 70000.
      Lh     = 2*a 
      Lv     = dsqrt(h**2+(b+a)**2)
      TOL    = 1e-3
      i      = 0
      uu(1)  = 0.
      uu(2)  = 0.
      R(1)   = 20000.
      R(2)   = 0.
      Beta   = 0.005
      pas    = 0.01
      TgSwap = 900.

C ----ParamŠtres----------   
      
      write(*,*)'TOL?'
      read(*,*)TOL
      write(*,*)'Beta?'
      read(*,*)Beta
      write(*,*)'ETA max?'
      read(*,*)ETAMAX
      write(*,*)'pas N.R.?'
      read(*,*)pas

C-------------------type=1 -> NR      
C                   type=2 -> RC
      type=1

C ----Initialisation------------------------------

      t=1
      x(1)=0. 
      y(1)=0. 
      Force(1)=0.

C  --------------------Newton Raphson----          
30    if (type.EQ.1) then     
         lambdat=lambda
         uut(1)=uu(1)
         uut(2)=uu(2)
C        ----Calcul de l'incr‚ment de Force----
         if (t.GT.1) then
            if ((Force(t-1)-Force(t)).LT.0) then
               lambda=lambda+pas
            else
               lambda=lambda-pas
            endif
         else
            lambda=lambda+pas
         endif
         boucle=0
         ii=0
20       u=uu(1) 
         v=uu(2)
         ii=ii+1
         F(1)=(E*Vv/Lv**4)*(v*v+u*u-2*h*v+2*u*(a+b))*(v-h)
         F(2)=(E*Vv/Lv**4)*(v**2+u**2-2*h*v+2*u*(a+b))*(u+a+b)
         F(2)=F(2)+(8*E*Vh/Lh**4)*(u**2+2*a*u)*(u+a)
         if (lambda.NE.0) then
            norm=dsqrt((lambda*R(1)-F(1))**2+(lambda*R(2)-F(2))**2)
     *           /abs(lambda)/dsqrt(R(1)**2+R(2)**2)
         else
            norm=0
         endif
         if ((norm.GT.TOL).OR.(ii.EQ.1)) then
            Kt(1,1)=E*Vv/(Lv**4)*(2*u+2*(a+b))*(v-h)
            Kt(1,2)=E*Vv/(Lv**4)*((v**2+u**2-2*v*h+2*u*(a+b))
     *              +(v-h)*(2*v-2*h))
            Kt(2,1)=E*Vv/(Lv**4)*((v**2+u**2-2*h*v+2*u*(a+b))
     *              +(u+a+b)*(2*u+2*(a+b)))+8*E*Vh/(Lh**4)*((2*u+2*a)
     *              *(u+a)+(u**2+2*a*u))
            Kt(2,2)=E*Vv/(Lv**4)*(u+a+b)*(2*v-2*h)
            MAT(1)=lambda*R(1)-F(1)
            MAT(2)=lambda*R(2)-F(2)
            dtmKt=Kt(1,1)*Kt(2,2)-Kt(1,2)*Kt(2,1)
            invKt(1,1)=Kt(2,2)/dtmKt
            invKt(2,2)=Kt(1,1)/dtmKt
            invKt(1,2)=-1*Kt(1,2)/dtmKt
            invKt(2,1)=-1*Kt(2,1)/dtmKt
            Du(1)=invKt(1,1)*MAT(1)+invKt(1,2)*MAT(2)
            Du(2)=invKt(2,1)*MAT(1)+invKt(2,2)*MAT(2)  
            if (ii.EQ.1) then    
               Du1(1)=Du(1)
               Du1(2)=Du(2)
            endif
            uu(1)=uu(1)+Du(1)
            uu(2)=uu(2)+Du(2)      
         else
            boucle=1
         endif
         if (boucle.EQ.0) goto 20
         t=t+1
         Force(t)=lambda*R(1)
         x(t)=uu(2)
         y(t)=uu(1)
         ETA=dsqrt(Du1(1)**2+Du1(2)**2)/
     *       dsqrt((uu(1)-uut(1))**2+(uu(2)-uut(2))**2)
      else
C  --------------------------Ricks Criesfield----
         i=0
         lambdat=lambda
         uut(1)=uu(1)
         uut(2)=uu(2)
         lambda=(Force(t))/R(1)
         boucle=0
40       u=uu(1) 
         v=uu(2)
         Fint(1)=(E*Vv/Lv**4)*(v*v+u*u-2*h*v+2*u*(a+b))*(v-h)
         Fint(2)=(E*Vv/Lv**4)*(v**2+u**2-2*h*v+2*u*(a+b))*(u+a+b)
         Fint(2)=Fint(2)+(8*E*Vh/Lh**4)*(u**2+2*a*u)*(u+a)
         norm=dsqrt((lambda*R(1)-Fint(1))**2+(lambda*R(2)-Fint(2))
     *           **2)/abs(lambda)/dsqrt(R(1)**2+R(2)**2)
         if ((norm.GT.TOL).OR.(i.EQ.0)) then
            i=i+1
            Kt(1,1)=E*Vv/(Lv**4)*(2*u+2*(a+b))*(v-h)
            Kt(1,2)=E*Vv/(Lv**4)*((v**2+u**2-2*v*h+2*u*(a+b))
     *              +(v-h)*(2*v-2*h))
            Kt(2,1)=E*Vv/(Lv**4)*((v**2+u**2-2*h*v+2*u*(a+b))
     *              +(u+a+b)*(2*u+2*(a+b)))+8*E*Vh/(Lh**4)*((2*u+2*a)
     *              *(u+a)+(u**2+2*a*u))
            Kt(2,2)=E*Vv/(Lv**4)*(u+a+b)*(2*v-2*h)
C           ----Calcul de Du1-----------
            MAT(1)=lambda*R(1)-Fint(1)
            MAT(2)=lambda*R(2)-Fint(2)
            dtmKt=Kt(1,1)*Kt(2,2)-Kt(1,2)*Kt(2,1)
            invKt(1,1)=Kt(2,2)/dtmKt
            invKt(2,2)=Kt(1,1)/dtmKt
            invKt(1,2)=-1*Kt(1,2)/dtmKt
            invKt(2,1)=-1*Kt(2,1)/dtmKt
            Du1(1)=invKt(1,1)*MAT(1)+invKt(1,2)*MAT(2)
            Du1(2)=invKt(2,1)*MAT(1)+invKt(2,2)*MAT(2)  
C           ----Calcul de Du2-----------
            Du2(1)=invKt(1,1)*R(1)+invKt(1,2)*R(2)
            Du2(2)=invKt(2,1)*R(1)+invKt(2,2)*R(2)  
C           ----Calcul de a1, a2, a3----
            ru(1)=uu(1)-uut(1)
            ru(2)=uu(2)-uut(2)
            ru2(1)=ru(1)+Du1(1)
            ru2(2)=ru(2)+Du1(2)
            a1=(Beta**2)*ps(R,R)+ps(Du2,Du2)
            a2=2*((lambda-lambdat)*(Beta**2)*ps(R,R)+ps(ru2,Du2))
            a3=2*ps(ru,Du1)+ps(Du1,Du1)+(ps(ru,ru)+
     *         ((lambda-lambdat)**2)*(Beta**2)*ps(R,R)-ETA**2)
            rac1=(-a2+dsqrt(a2**2-4*a1*a3))/2/a1
            rac2=(-a2-dsqrt(a2**2-4*a1*a3))/2/a1
            test1=uu(2)+Du1(2)+rac1*Du2(2) 
            test2=uu(2)+Du1(2)+rac2*Du2(2)  
            if ((test2-test1).GT.0) then
               uu(1)=uu(1)+Du1(1)+rac2*Du2(1)
               uu(2)=uu(2)+Du1(2)+rac2*Du2(2)  
               lambda=lambda+rac2
            else
               uu(1)=uu(1)+Du1(1)+rac1*Du2(1)
               uu(2)=uu(2)+Du1(2)+rac1*Du2(2)  
               lambda=lambda+rac1
            endif         
         else
            boucle=1
         endif
         if (boucle.EQ.0) goto 40

         ETA=ETA*dsqrt(4./i)
         if (ETA.GT.ETAMAX) then 
            ETA=ETAMAX
         endif
         t=t+1
         Force(t)=lambda*R(1)
         x(t)=uu(2)
         y(t)=uu(1)
      endif
C     ----Teste la tangente … la courbe:
C                     TG > 900 --> N.R.
C                     TG < 900---> R.C.
      if (abs((Force(t-1)-Force(t))/(uut(2)-uu(2))).LT.TgSwap) then
         type=2
      else
         type=1
      endif
      if (lambda.LT.1) goto 30

C ----Resultats-----------  
      
      open (UNIT = 1, FILE = 'rcnr_1.m', STATUS='unknown') 
      do 31 i=1,t
         write(1,*)'x(',i,')=',x(i),';'
         write(1,*)'y(',i,')=',y(i),';'  
         write(1,*)'P(',i,')=',Force(i),';'  
31    continue
      write(1,*)'figure(1), plot(x,y), grid'
      write(1,*)'figure(2), plot(x,P), grid'
      close (UNIT=1)
      write(*,*)'t =',t
      end

      double precision function ps(a,b)
         double precision a(2),b(2)
         ps=a(1)*b(1)+a(2)*b(2)
      return
      end
