C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     Projet MICO - Riks Crisfield (ver. FORTRAN 21.03.95)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


C ----Declarations--------
      integer i, boucle, t, signe
      double precision h, a, b, Vh, Vv, E, Lh, Lv, prec
      double precision TOL, u, Force(1000), Du1(2)
      double precision lambda, Beta, ETA, Du2(2), si, sit
      double precision R(2), uu(2)
      double precision norm, Kt(2,2), MAT(2), dudv
      double precision invKt(2,2), dtmKt, uut(2), lambdat, Fint(2), dpdv
      double precision rac1, rac2, a1, a2, a3, test1, test2, den
      double precision ps, ru(2), ru2(2)

C ----Donnees-------------     
      h     = 40. 
      a     = 24. 
      b     = 40. 
      Vh    = 160. 
      Vv    = 200. 
      E     = 70000.
      Lh    = 2*a 
      Lv    = dsqrt(h**2+(b+a)**2)
      TOL   = 1e-3
      i     = 0
      uu(1) = 0.
      uu(2) = 0.
      R(1)  = 20000.
      R(2)  = 0.
      Beta  = 0.005

      write(*,*)'Pr‚cision?'
      read(*,*)prec

C ----Initialisation------------------------------

      t        = 1
      Force(1) = 0.
      sit      = 1
      ETA      = 32.

C ----Ricks Crisfield-----------------------------

      lambda=Force(t)/R(1)
30    i=0
      lambdat=lambda
      uut(1)=uu(1)
      uut(2)=uu(2)
      lambda=(Force(t)+ETA/Beta)/R(1)
      boucle=0
      u=uu(1)
      v=uu(2)
C     ----Calcul de la d‚riv‚e de P par rapport … v----
      den=Vv/Lv**4*(v*v+u*u-2*h*v+2*u*(a+b)+2*(a+b+u)**2)
      den=den+8*Vh/Lh**4*(2*(u+a)**2+u*(u+2*a))
      dudv=Vv/Lv**4*2*(a+b+u)*(v-h)/den
      dpdv=E*Vv/Lv**4*(v-h)*(2*v-2*h+2*(u+a+b)*dudv)
      dpdv=dpdv+(v*v+u*u-2*h*v+2*u*(a+b))*E*Vv/Lv**4
C     ----R‚duit ETA aprŠs un changement de signe----
      si=signe(dpdv)
      if (si-sit.NE.0) then
         ETA=ETA/2
      endif
      sit=si   
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
            test2=uu(2)+Du1(2)+rac2*Du2(2)  
            test1=uu(2)+Du1(2)+rac1*Du2(2)
C           ----CritŠre du choix de la racine----
            if(dpdv.gt.0) then
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
               if ((test2-test1).LT.0) then
                  uu(1)=uu(1)+Du1(1)+rac2*Du2(1)
                  uu(2)=uu(2)+Du1(2)+rac2*Du2(2)  
                  lambda=lambda+rac2
               else
                  uu(1)=uu(1)+Du1(1)+rac1*Du2(1)
                  uu(2)=uu(2)+Du1(2)+rac1*Du2(2)  
                  lambda=lambda+rac1
               endif         
           endif
         else
            boucle=1
         endif
         if (boucle.EQ.0) goto 40
         t=t+1
         Force(t)=lambda*R(1)
      if (abs(dpdv).GT.prec) goto 30


C ----Resultats-----------  
       write(*,*)'Maximum     =',Force(t)    
       write(*,*)'LambdaCr    =',lambda
       write(*,*)'R           =',R(1)
       write(*,*)'(Umax;Vmax) =(',uu(1),';',uu(2),')'
      end

      double precision function ps(a,b)
         double precision a(2),b(2)
         ps=a(1)*b(1)+a(2)*b(2)
      return
      end

      integer function signe(x)
      double precision x
        signe=x/abs(x)
        return
      end
