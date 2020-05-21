C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C                       Fluides géophysiques  v14         (11.04.96)
C                       -------------------------
C   . Red Black
C   . Calcul des vitesses
C   . Sauvegarde tous les X pas de temps
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


C ------------------------------------------------------------------
C                            Declarations
C ------------------------------------------------------------------      
      double precision U, g, f
      integer pasx, pasy, pasx2, Nmax
      parameter(U=1D-1, g=1D-2, f=1D-4)
      parameter(pasx=31, pasx2=32, pasy=31)
      integer i, j, n, fin, compt, startj, start, save, fsave, saveu
      double precision pi, h0, L, T, Lx, Ly, Bu, Ro, Et, w
      double precision dx, dy, dt, tol, x(pasx2), y(pasx), kmoy
      double precision temp1, temp2, temp3, temp4, temp5, erreur
      double precision eta(pasx2,pasy), b(pasx2,pasy), q(pasx2,pasy)
      double precision aeta1, aeta2, jacob(pasx2,pasy), deta(pasx2,2)
      double precision leta(pasx2,pasy), v1(pasx2,pasy), v2(pasx2,pasy)

C ------------------------------------------------------------------
C                            Donnees
C ------------------------------------------------------------------

      write(*,*)'L (5000)?'
      read(*,*)L
      write(*,*)'Lx/L (5)?'
      read(*,*)Lx

      pi    = acos(-1.)
      h0    = 1D2
      T     = L/U
      Ly    = amin1((3./4.+g*h0/(2.*f*U*L)), 4.)
      Bu    = g*h0/(f*L)**2
      Ro    = U/(f*L) 
      Et    = 1/(f*T)
      dx    = 2.*Lx/(pasx-1) 
      dt    = dx*dx/2.
      dy    = 2.*Ly/(pasy-1)
      kmoy  = 0.
      save  = 1

      write(*,*)'Ly/L =',Ly,'(',(3./4.+g*h0/(2.*f*U*L)),')'
      write(*,*)'Bu   =',Bu,'Ro   =',Ro
      write(*,*)'dx   =',dx,' ; dy   =',dy 
      write(*,*)'pasx =',pasx,' ; pasy =',pasy
      write(*,*)'T    =',T,'dH   =',f*L*U/g
      write(*,*)'dt (max) =',dt,'sec.' 
      write(*,*)'Le canal fait ',2*Lx*L,'mètres de long'
      write(*,*)'           et ',2*Ly*L,'mètres de large'
      write(*,*)'tol (1D-6)?'
      read(*,*)tol
      write(*,*)'w (1.825)?'
      read(*,*)w
      write(*,*)'dt (2D-2)?'
      read(*,*)dt
      write(*,*)'Nmax (100)?'
      read(*,*)Nmax
      write(*,*)'fsave (Nmax)?'
      read(*,*)fsave
      write(*,*)'save U (1 = oui)?'
      read(*,*)saveu

      if(fsave.GT.Nmax) then
         fsave=Nmax
      endif

      write(*,*)
      write(*,*)'Un pas de temps =',T*dt,'sec.'

C ------------------------------------------------------------------      
C                      Calcul du profil initial
C ------------------------------------------------------------------      
      
      do 10 i=2,pasx+1
         x(i)=(i-2)*dx
10    continue
      x(1)=x(pasx)
      x(2)=x(pasx+1)
    
      do 20 j=1,pasy      
         y(j)=-Ly+(j-1)*dy
20    continue

      temp1 = g*h0/f/L/U
      do 30 i=1,pasx+1
         temp2 = 0.01*sin(pi*x(i)/Lx)
         do 30 j=1,pasy
            if(abs(y(j)).LT.1) then
               eta(i,j)=temp1*temp2-0.5*y(j)**2*(1+temp2)
            else
               eta(i,j)=temp1*temp2+(0.5-abs(y(j)))*(1+temp2)
            endif
30    continue

C ------------------------------------------------------------------      
C       Ouverture du fichier RES.M et sauv. le profil initial
C ------------------------------------------------------------------

      open (UNIT = 1, FILE = 'res_orig.m', STATUS ='unknown')

      do 111 i=2,pasx+1
         do 111 j=1,pasy
            write(1,*)'a(',(save-1)*pasx+i-1,',',j,')=',eta(i,j),';'
111   continue

C ------------------------------------------------------------------      
C            Boucle principale (progression dans le temps)
C ------------------------------------------------------------------
      
      do 40 n=1,Nmax
      write(*,*)'Pas de temps n :',n

C        ------------------------------------
C        -- Calcul de Beta et du laplacien --
C        ------------------------------------

C        -- Interieur --

      do 50 i=2,pasx
         do 50 j=2,pasy-1
            temp1=(eta(i-1,j)+eta(i+1,j)-2*eta(i,j))/(dx*dx) 
     *            +(eta(i,j-1)+eta(i,j+1)-2*eta(i,j))/(dy*dy)
            temp2=((eta(i+1,j)-eta(i-1,j))/(2*dx))**2 
     *            +((eta(i,j+1)-eta(i,j-1))/(2*dy))**2
            b(i,j)=(Bu+Ro*eta(i,j))*temp1+Ro/2*temp2
            q(i,j)=temp1
50    continue
     
C        -- Bords y --

      do 60 i=2,pasx
         j=1
         temp1=(eta(i-1,j)+eta(i+1,j)-2*eta(i,j))/(dx*dx) 
     *         +(eta(i,j)+eta(i,j+2)-2*eta(i,j+1))/(dy*dy)
         temp2=((eta(i+1,j)-eta(i-1,j))/(2*dx))**2 
     *         +((4*eta(i,j+1)-eta(i,j+2)-3*eta(i,j))/(2*dy))**2
         b(i,j)=(Bu+Ro*eta(i,j))*temp1+Ro/2*temp2
         q(i,j)=temp1
         j=pasy
         temp1=(eta(i-1,j)+eta(i+1,j)-2*eta(i,j))/(dx*dx) 
     *         +(eta(i,j)+eta(i,j-2)-2*eta(i,j-1))/(dy*dy)
         temp2=((eta(i+1,j)-eta(i-1,j))/(2*dx))**2 
     *         +((-4*eta(i,j-1)+eta(i,j-2)+3*eta(i,j))/(2*dy))**2
         b(i,j)=(Bu+Ro*eta(i,j))*temp1+Ro/2*temp2
         q(i,j)=temp1
60    continue
     
C        -- Ajout des lignes i=1 et i=pasx+1 --

      do 70 j=1,pasy
         b(1,j)=b(pasx,j)
         b(pasx+1,j)=b(2,j)
         q(1,j)=q(pasx,j)
         q(pasx+1,j)=q(2,j)
70    continue

C        ------------------------     
C        -- Calcul du Jacobien --
C        ------------------------

      do 80 i=2,pasx
         do 80 j=2,pasy-1
         temp1=((eta(i+1,j)-eta(i-1,j))*(b(i,j+1)-b(i,j-1))-
     *          (eta(i,j+1)-eta(i,j-1))*(b(i+1,j)-b(i-1,j)))/4/dx/dy
         temp2=(eta(i+1,j)*(b(i+1,j+1)-b(i+1,j-1))-eta(i-1,j)*
     *          (b(i-1,j+1)-b(i-1,j-1))-eta(i,j+1)*(b(i+1,j+1)
     *          -b(i-1,j+1))+eta(i,j-1)*(b(i+1,j-1)-b(i-1,j-1)))
     *          /4/dx/dy
         temp3=(b(i,j+1)*(eta(i+1,j+1)-eta(i-1,j+1))-b(i,j-1)*
     *          (eta(i+1,j-1)-eta(i-1,j-1))-b(i+1,j)*(eta(i+1,j+1)-
     *          eta(i+1,j-1))+b(i-1,j)*(eta(i-1,j+1)-eta(i-1,j-1)))
     *          /4/dx/dy
         jacob(i,j)=(temp1+temp2+temp3)/3
80    continue
     
C        -- Ajout des lignes i=1 et i=pasx+1 --
      
      do 90 j=1,pasy
         jacob(1,j)=jacob(pasx,j)
         jacob(pasx+1,j)=jacob(2,j)
90    continue
     
C        -----------------      
C        -- Calcul de q --
C        -----------------      

      do 100 i=1,pasx+1
        do 100 j=1,pasy
           q(i,j)=Bu*q(i,j)-eta(i,j)           
           q(i,j)=q(i,j)-Ro/Et*dt*jacob(i,j)   
100   continue

C        ----------------------------------------     
C        -- Calcul de d(eta)/dy pour les bords --
C        ----------------------------------------
      
      do 110 i=1,pasx+1
         deta(i,1)=(4*eta(i,2)-eta(i,3)-3*eta(i,1))/(2*dy)
         deta(i,2)=(-4*eta(i,pasy-1)+eta(i,pasy-2)+3*eta(i,pasy))
     *              /(2*dy)
110   continue
     
C        ---------------
C        -- Red Black --
C        ---------------

      fin=0
      compt=0
120   compt=compt+1
      fin=1
C                 - Deux passages alternes -
      do 130 startj=1,2
         start=startj      
C                       - Red Black -
         do 140 i=2,pasx
            do 150 j=start+1,pasy-1,2
               temp1=-2/dx**2-2/dy**2-1/Bu
               temp1=(q(i,j)/Bu-(eta(i+1,j)+eta(i-1,j))/dx**2
     *               -(eta(i,j+1)+eta(i,j-1))/dy**2)/temp1
               temp1=eta(i,j)+w*(temp1-eta(i,j))
               if(abs(eta(i,j)).GT.0) then
                  erreur=abs((temp1-eta(i,j))/eta(i,j))
                  if(erreur.GT.tol) then
                     fin=0
                  endif
               endif
               eta(i,j)=temp1 
150         continue

            if(start.EQ.2) then
               start=1
            else 
               start=2
            endif
140      continue

         do 160 j=1,pasy
            eta(1,j)=eta(pasx,j)
            eta(pasx+1,j)=eta(2,j)   
160      continue

130   continue

C                  - Conditions aux limites -
      aeta1=eta(1,1)
      aeta2=eta(1,pasy)

      do 170 i=2,pasx
         j=1
         temp1=(eta(i+1,j)-aeta1)/(2*dx)
         temp2=(eta(i,j+2)-2*eta(i,j+1))/dy**2
          d1=(eta(i+1,j)-aeta1)/(2*dx)
          d2=(eta(i+1,j+1)-eta(i-1,j+1))/(2*dx)
          d3=(eta(i+1,j+2)-eta(i-1,j+2))/(2*dx)
         temp3=(4*d2-d3-3*d1)/(2*dy)              
         temp4=-3*Et/(2*dy*dt)+3*Ro*temp3/(2*dy)+Ro*temp1/dy**2
         temp5=(temp1-Et*((4*eta(i,j+1)-eta(i,j+2))/(2*dy*dt)-
     *         deta(i,1)/dt)-Ro*temp1*temp2+Ro*temp3*
     *         (4*eta(i,j+1)-eta(i,j+2))/(2*dy))/temp4
         temp5=eta(i,j)+w*(temp5-eta(i,j))
         if(abs(eta(i,j)).GT.0) then
            erreur=abs((temp5-eta(i,j))/eta(i,j))
            if(erreur.GT.tol) then
               fin=0
            endif
         endif
         aeta1=eta(i,j)
         eta(i,j)=temp5
      
         j=pasy
         temp1=(eta(i+1,j)-aeta2)/(2*dx)
         temp2=(eta(i,j-2)-2*eta(i,j-1))/dy**2
          d1=(eta(i+1,j)-aeta2)/(2*dx)
          d2=(eta(i+1,j-1)-eta(i-1,j-1))/(2*dx)
          d3=(eta(i+1,j-2)-eta(i-1,j-2))/(2*dx)
         temp3=(-4*d2+d3+3*d1)/(2*dy)   
         temp4=3*Et/(2*dy*dt)-3*Ro*temp3/(2*dy)+Ro*temp1/dy**2
         temp5=(temp1-Et*((-4*eta(i,j-1)+eta(i,j-2))/(2*dy*dt)
     *         -deta(i,2)/dt)-Ro*temp1*temp2+Ro*temp3*
     *         (-4*eta(i,j-1)+eta(i,j-2))/(2*dy))/temp4
         temp5=eta(i,j)+w*(temp5-eta(i,j))
         if(abs(eta(i,j)).GT.0) then
            erreur=abs((temp5-eta(i,j))/eta(i,j))
            if(erreur.GT.tol) then
               fin=0
            endif
         endif
         aeta2=eta(i,j)
         eta(i,j)=temp5
170   continue
C                         -  Periodicite -
      do 180 j=1,pasy
         eta(1,j)=eta(pasx,j)
         eta(pasx+1,j)=eta(2,j)      
180   continue

      if(fin.EQ.0) goto 120  
       
      write(*,*)'    no it. =',compt
      kmoy=kmoy+compt

C        ----------------------------------------------
C        -- Copie de eta (futur calcul des vitesses) --
C        ----------------------------------------------

      if(n.EQ.save*fsave-1) then
         do 190 i=1,pasx2
            do 190 j=1,pasy
               leta(i,j)=eta(i,j)
190      continue
      endif

C        -------------------------
C        -- Calcul des vitesses --
C        -------------------------
      if(saveu.EQ.1) then
      if(n.EQ.save*fsave) then
      do 210 i=2,pasx
         do 200 j=2,pasy-1

C                        - u (interieur) -            

            temp1=(eta(i,j+1)-eta(i,j-1))/2/dy
            temp2=(eta(i+1,j)-eta(i-1,j))/2/dx
            temp3=(leta(i+1,j)-leta(i-1,j))/2/dx
            temp4=(eta(i+1,j+1)-eta(i-1,j+1)-eta(i+1,j-1)
     *            +eta(i-1,j-1))/(4*dy*dy)
            temp5=(eta(i+1,j)+eta(i-1,j)-2*eta(i,j))/(dx*dx)
            v1(i,j)=-temp1-Et*(temp2-temp3)/dt-Ro*
     *             (temp2*temp4-temp1*temp5)

C                        - v (interieur) -      

            temp3=(leta(i,j+1)-leta(i,j-1))/2/dy
            temp5=(eta(i,j+1)+eta(i,j-1)-2*eta(i,j))/(dy*dy)
            v2(i,j)=temp2-Et*(temp1-temp3)/dt-Ro*
     *             (temp2*temp5-temp1*temp4)
200       continue
          j=1

C                          - u (j=1) -            

            temp1=(4*eta(i,j+1)-eta(i,j+2)-3*eta(i,j))/2/dy
            temp2=(eta(i+1,j)-eta(i-1,j))/2/dx
            temp3=(leta(i+1,j)-leta(i-1,j))/2/dx
             d1=temp2
             d2=(eta(i+1,j+1)-eta(i-1,j+1))/(2*dx)
             d3=(eta(i+1,j+2)-eta(i-1,j+2))/(2*dx)
            temp4=(4*d2-d3-3*d1)/(2*dy)              
            temp5=(eta(i+1,j)+eta(i-1,j)-2*eta(i,j))/(dx*dx)
            v1(i,j)=-temp1-Et*(temp2-temp3)/dt-Ro*
     *             (temp2*temp4-temp1*temp5)

C                          - v (j=1) -      

            temp3=(4*leta(i,j+1)-leta(i,j+2)-3*leta(i,j))/2/dy 
            temp5=(eta(i,j+2)+eta(i,j)-2*eta(i,j+1))/(dy*dy)
            v2(i,j)=temp2-Et*(temp1-temp3)/dt-Ro*
     *             (temp2*temp5-temp1*temp4)
          j=pasy
           
C                         - u (j=pasy) -            

            temp1=(-4*eta(i,j-1)+eta(i,j-2)+3*eta(i,j))/2/dy
            temp2=(eta(i+1,j)-eta(i-1,j))/2/dx
            temp3=(leta(i+1,j)-leta(i-1,j))/2/dx
             d1=temp2
             d2=(eta(i+1,j-1)-eta(i-1,j-1))/(2*dx)
             d3=(eta(i+1,j-2)-eta(i-1,j-2))/(2*dx)
            temp4=(-4*d2+d3+3*d1)/(2*dy)   
            temp5=(eta(i+1,j)+eta(i-1,j)-2*eta(i,j))/(dx*dx)
            v1(i,j)=-temp1-Et*(temp2-temp3)/dt-Ro*
     *             (temp2*temp4-temp1*temp5)

C                         - v (j=pasy) -      

            temp3=(-4*leta(i,j-1)+leta(i,j-2)+3*leta(i,j))/2/dy 
            temp5=(eta(i,j-2)+eta(i,j)-2*eta(i,j-1))/(dy*dy)
            v2(i,j)=temp2-Et*(temp1-temp3)/dt-Ro*
     *             (temp2*temp5-temp1*temp4)
210   continue

C                         - periodicite -
      do 220 j=1,pasy
         v1(pasx+1,j)=v1(2,j)
         v2(pasx+1,j)=v2(2,j)
220   continue
      endif
      endif

C        --------------------------------
C        -- Resultats (save -> Matlab) --
C        --------------------------------

      if(n.EQ.save*fsave) then     
         do 31 i=2,pasx+1
            do 31 j=1,pasy
               write(1,*)'a(',(save)*pasx+i-1,',',j,')=',eta(i,j),';'
               if(saveu.EQ.1) then
                  write(1,*)'u(',(save)*pasx+i-1,',',j,')=',v1(i,j),';'
                  write(1,*)'v(',(save)*pasx+i-1,',',j,')=',v2(i,j),';'
               endif
31       continue
         save=save+1
      endif

40    continue  
      close (UNIT=1)

C ------------------------------------------------------------------
C                     Commentaires finaux
C ------------------------------------------------------------------

      write(*,*)'Calcul effectué sur ',Nmax*T*dt,'sec.'
      write(*,*)'Nb itérations (moy) ',kmoy/Nmax
      write(*,*)'Résultats dans RES.M'
      
      end
