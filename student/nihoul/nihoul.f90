! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                       Fluides géophysiques  v14         (11.04.96)
!                       -------------------------
!   . Red Black
!   . Calcul des vitesses
!   . Sauvegarde tous les X pas de temps
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

program NIHOUL

    implicit none

    ! Declarations

    double precision, parameter :: U = 1D-1
    double precision, parameter :: g = 1D-2
    double precision, parameter :: f = 1D-4

    integer, parameter :: pasx  = 31
    !integer, parameter :: pasx2 = 32    
    integer, parameter :: pasy  = 31

    integer :: Nmax
    logical :: fin
    integer :: i, j, n, compt, startj, start, nsave, fsave, saveu
    double precision :: pi, h0, L, T, Lx, Ly, Bu, Ro, Et, w
    double precision :: dx, dy, dt, tol, kmoy
    !double precision :: x(pasx2), y(pasx)
    double precision, allocatable :: x(:), y(:)
    
    double precision :: temp1, temp2, temp3, temp4, temp5, erreur
    double precision :: d1, d2, d3
    double precision :: aeta1, aeta2     
    
    !double precision :: eta(pasx2,pasy), b(pasx2,pasy), q(pasx2,pasy)
    !double precision :: jacob(pasx2,pasy), deta(pasx2,2)
    !double precision :: leta(pasx2,pasy), v1(pasx2,pasy), v2(pasx2,pasy)
    double precision, allocatable :: eta(:,:), b(:,:), q(:,:)
    double precision, allocatable :: jacob(:,:), deta(:,:)
    double precision, allocatable :: leta(:,:), v1(:,:), v2(:,:)

    ! dynamic arrays
    allocate( x(pasx+1) )
    allocate( y(pasx) )

    allocate( eta(pasx+1, pasy) )
    allocate( b(pasx+1, pasy) )
    allocate( q(pasx+1, pasy) )

    allocate( jacob(pasx+1, pasy) )
    allocate( deta(pasx+1, 2) )

    allocate( leta(pasx+1, pasy) )
    allocate( v1(pasx+1, pasy) )
    allocate( v2(pasx+1, pasy) )

    ! Donnees

    print *, 'L (5000)?'
    read(*,*) L
    print *, 'Lx/L (5)?'
    read(*,*) Lx

    pi    = acos(-1.0D0)
    h0    = 1D2
    T     = L/U
    Ly    = min((3.0D0/4.0D0 + g*h0/(2.0D0*f*U*L)), 4.0D0)
    Bu    = g*h0/(f*L)**2
    Ro    = U/(f*L) 
    Et    = 1.0D0/(f*T)
    dx    = 2.0D0*Lx/(pasx-1) 
    dt    = dx*dx/2.0D0
    dy    = 2.0D0*Ly/(pasy-1)
    kmoy  = 0.0D0
    nsave  = 1

    print *, 'Ly/L =', Ly, '(', (3.0D0/4.0D0 + g*h0/(2.0D0*f*U*L)), ')'
    print *, 'Bu   =', Bu, ' ; Ro   =', Ro
    print *, 'dx   =', dx, ' ; dy   =', dy 
    print *, 'pasx =', pasx,' ; pasy =', pasy
    print *, 'T    =', T, ' ; dH   =', f*L*U/g
    print *, 'dt (max) =', dt, 'sec.' 
    print *, 'Le canal fait ', 2.0D0*Lx*L, 'm de long'
    print *, '           et ', 2.0D0*Ly*L, 'm de large'
    print *, 'tol (1D-6)?'
    read(*,*) tol
    print *, 'w (1.825)?'
    read(*,*) w
    print *, 'dt (2D-2)?'
    read(*,*) dt
    print *, 'Nmax (100)?'
    read(*,*) Nmax
    print *, 'fsave (Nmax)?'
    read(*,*) fsave
    print *, 'save U (1 = oui)?'
    read(*,*) saveu

    if(fsave > Nmax) then
        fsave = Nmax
    endif

    write(*,*)
    print *, 'Un pas de temps =', T*dt, 'sec.'

    ! Calcul du profil initial
      
    do i = 2, pasx+1
        x(i) = (i-2)*dx
    end do

    x(1) = x(pasx)
    x(2) = x(pasx+1)

    do j = 1, pasy      
        y(j) = -Ly + (j-1)*dy
    end do

    temp1 = g*h0/f/L/U
    do i = 1, pasx+1
        temp2 = 0.01*sin(pi*x(i)/Lx)
        do j = 1, pasy
            if (abs(y(j)) < 1.0D0) then
                eta(i,j) = temp1*temp2 - 0.5*y(j)**2 * (1 + temp2)
            else
                eta(i,j) = temp1*temp2 + (0.5-abs(y(j))) * (1 + temp2)
            endif
        end do
    end do

    ! Ouverture du fichier RES.M et sauvegarde du profil initial

    open (UNIT = 1, FILE = 'res.m', STATUS ='unknown')

    do i = 2, pasx+1
        do j = 1, pasy
            write(1,*) 'a(', (nsave-1)*pasx+i-1, ',', j, ')=', eta(i,j), ';'
        end do
    end do

    ! Boucle principale (progression dans le temps)

    do n = 1, Nmax
        print *, 'Pas de temps n :', n

        ! Calcul de Beta et du laplacien

        ! Interieur

        do i = 2, pasx
            do j = 2, pasy-1
                temp1 = (eta(i-1,j) + eta(i+1,j) - 2*eta(i,j))/(dx*dx)        &
                      + (eta(i,j-1) + eta(i,j+1) - 2*eta(i,j))/(dy*dy)
                temp2 = ((eta(i+1,j) - eta(i-1,j))/(2*dx))**2               &
                      + ((eta(i,j+1) - eta(i,j-1))/(2*dy))**2
                b(i,j) = (Bu+Ro*eta(i,j))*temp1 + Ro/2*temp2
                q(i,j) = temp1
            end do
        end do
     
        ! Bords y

        do i = 2, pasx
            j = 1
            temp1 = (eta(i-1,j) + eta(i+1,j) - 2*eta(i,j))   /(dx*dx)       &
                  + (eta(i,j)   + eta(i,j+2) - 2*eta(i,j+1)) /(dy*dy)
            temp2 = ((  eta(i+1,j) - eta(i-1,j)) / (2*dx))**2               &
                  + ((4*eta(i,j+1) - eta(i,j+2) - 3*eta(i,j)) / (2*dy))**2
            b(i,j) = (Bu+Ro*eta(i,j))*temp1 + Ro/2*temp2
            q(i,j) = temp1

            j = pasy
            temp1 = (eta(i-1,j) + eta(i+1,j) - 2*eta(i,j))   /(dx*dx)       &
                  + (eta(i,j)   + eta(i,j-2) - 2*eta(i,j-1)) /(dy*dy)
            temp2 = ((eta(i+1,j) - eta(i-1,j))/(2*dx))**2                     &
                  + ((-4*eta(i,j-1) + eta(i,j-2) + 3*eta(i,j)) / (2*dy))**2
            b(i,j) = (Bu+Ro*eta(i,j))*temp1 + Ro/2*temp2
            q(i,j) = temp1
        end do
     
        ! Ajout des lignes i = 1 et i = pasx+1

        do j = 1, pasy
            b(1,j) = b(pasx,j)
            b(pasx+1,j) = b(2,j)
            q(1,j) = q(pasx,j)
            q(pasx+1,j) = q(2,j)
        end do    

        ! Calcul du Jacobien

        do i = 2, pasx
            do j = 2, pasy-1
                temp1 = ((eta(i+1,j)-eta(i-1,j))*(b(i,j+1)-b(i,j-1))-          &
                (eta(i,j+1) - eta(i,j-1))*(b(i+1,j) - b(i-1,j)))/4/dx/dy
                temp2 = (eta(i+1,j)*(b(i+1,j+1) - b(i+1,j-1)) - eta(i-1,j)*        &
                    (b(i-1,j+1) - b(i-1,j-1)) - eta(i,j+1)*(b(i+1,j+1)             &
                            -b(i-1,j+1)) + eta(i,j-1)*(b(i+1,j-1) - b(i-1,j-1)))   &
                                    /4/dx/dy
                temp3 = (b(i,j+1)*(eta(i+1,j+1) - eta(i-1,j+1)) - b(i,j-1)*        &
                    (eta(i+1,j-1) - eta(i-1,j-1)) - b(i+1,j)*(eta(i+1,j+1)-        &
                    eta(i+1,j-1)) + b(i-1,j)*(eta(i-1,j+1) - eta(i-1,j-1)))        &
                    /4/dx/dy
                jacob(i,j) = (temp1 + temp2 + temp3)/3.0D0
            end do
        end do
     
        ! Ajout des lignes i = 1 et i = pasx+1
      
        do j = 1,pasy
            jacob(1,j) = jacob(pasx,j)
            jacob(pasx+1,j) = jacob(2,j)
        end do
          
        ! Calcul de q
      
        do i = 1, pasx+1
            do j = 1, pasy
                q(i,j) = Bu*q(i,j) - eta(i,j)           
                q(i,j) = q(i,j) - Ro/Et*dt*jacob(i,j)   
            end do
        end do

        ! Calcul de d(eta)/dy pour les bords

        do i = 1, pasx+1
            deta(i,1) = (4*eta(i,2) - eta(i,3) - 3*eta(i,1)) / (2*dy)
            deta(i,2) = (-4*eta(i,pasy-1) + eta(i,pasy-2) + 3*eta(i,pasy)) / (2*dy)
        end do
     
        ! Red Black

        fin = .false.
        compt = 0

        do while (fin == 0)
            compt = compt + 1
            fin = .true.

            ! Deux passages alternes

            do startj = 1, 2
                start = startj

                ! Red Black

                do i = 2, pasx
                    do j = start+1, pasy-1, 2
                        temp1 = -2/dx**2 - 2/dy**2 - 1/Bu
                        temp1 = (q(i,j)/Bu - (eta(i+1,j)+eta(i-1,j))/dx**2   &
                                           - (eta(i,j+1)+eta(i,j-1))/dy**2  ) / temp1
                        temp1 = eta(i,j) + w*(temp1-eta(i,j))
                        if(abs(eta(i,j)) > 0.0D0) then
                            erreur = abs( (temp1-eta(i,j))/eta(i,j) )
                            if(erreur > tol) then
                                fin = .false.
                            endif
                        endif
                        eta(i,j) = temp1 
                    end do

                    if(start == 2) then
                        start = 1
                    else 
                        start = 2
                    endif
                end do

                do j = 1, pasy
                    eta(1,j) = eta(pasx,j) 
                    eta(pasx+1,j) = eta(2,j)   
                end do

            end do

            ! Conditions aux limites

            aeta1 = eta(1,1)
            aeta2 = eta(1,pasy)

            do i = 2, pasx
                j = 1
                temp1 = (eta(i+1,j)-aeta1) / (2*dx)
                temp2 = (eta(i,j+2)-2*eta(i,j+1)) / dy**2
                d1 = (eta(i+1,j)-aeta1) / (2*dx)
                d2 = (eta(i+1,j+1)-eta(i-1,j+1))/(2*dx)
                d3 = (eta(i+1,j+2)-eta(i-1,j+2))/(2*dx)
                temp3 = (4*d2-d3-3*d1)/(2*dy)              
                temp4 = -3*Et/(2*dy*dt)+3*Ro*temp3/(2*dy)+Ro*temp1/dy**2
                temp5 = (temp1  -Et*((4*eta(i,j+1) - eta(i,j+2)) / (2*dy*dt)    &
                        -deta(i,1)/dt) - Ro*temp1*temp2 + Ro*temp3*             &
                        (4*eta(i,j+1) - eta(i,j+2))/(2*dy) ) / temp4
                temp5 = eta(i,j) + w*(temp5-eta(i,j))
                if(abs(eta(i,j)) > 0.0D0) then
                    erreur = abs( (temp5-eta(i,j))/eta(i,j) )
                    if(erreur > tol) then
                        fin = .false.
                    endif
                endif
                aeta1 = eta(i,j)
                eta(i,j) = temp5

                j = pasy
                temp1 = (eta(i+1,j) - aeta2) / (2*dx)
                temp2 = (eta(i,j-2) - 2*eta(i,j-1)) / dy**2
                d1 = (eta(i+1,j) - aeta2) / (2*dx)
                d2 = (eta(i+1,j-1) - eta(i-1,j-1)) / (2*dx)
                d3 = (eta(i+1,j-2) - eta(i-1,j-2)) / (2*dx)
                temp3 = (-4*d2 + d3 + 3*d1) / (2*dy)   
                temp4 = 3*Et/(2*dy*dt) - 3*Ro*temp3/(2*dy) + Ro*temp1/dy**2
                temp5 = (temp1 - Et*((-4*eta(i,j-1) + eta(i,j-2)) / (2*dy*dt)    &
                        - deta(i,2)/dt) - Ro*temp1*temp2 + Ro*temp3*             &
                        (-4*eta(i,j-1) + eta(i,j-2))/(2*dy) ) / temp4
                temp5 = eta(i,j) + w*(temp5-eta(i,j))
                if (abs(eta(i,j)) > 0.0D0) then
                    erreur = abs( (temp5-eta(i,j)) / eta(i,j) )
                    if (erreur > tol) then
                        fin = .false.
                    endif
                endif
                aeta2 = eta(i,j)
                eta(i,j) = temp5
            end do

            ! Périodicité
            do j = 1, pasy
                eta(1,j) = eta(pasx,j)
                eta(pasx+1,j) = eta(2,j)      
            end do

        end do  
       
        print *, '    no it. =', compt
        kmoy = kmoy + compt

        ! Copie de eta (futur calcul des vitesses)

        if(n == nsave*fsave-1) then
            do i = 1, pasx+1
                do j = 1, pasy
                    leta(i,j) = eta(i,j)
                end do
            end do
        endif

        ! Calcul des vitesses

        if(saveu == 1) then
            if(n == nsave*fsave) then
                do i = 2, pasx
                    do j = 2, pasy-1

                        ! u (interieur)            

                        temp1 = (eta(i,j+1) - eta(i,j-1))/2/dy
                        temp2 = (eta(i+1,j) - eta(i-1,j))/2/dx
                        temp3 = (leta(i+1,j) - leta(i-1,j))/2/dx
                        temp4 = (eta(i+1,j+1) - eta(i-1,j+1) - eta(i+1,j-1)     &
                                + eta(i-1,j-1)) / (4*dy*dy)
                        temp5 = (eta(i+1,j) + eta(i-1,j) - 2*eta(i,j)) / (dx*dx)
                        v1(i,j) = -temp1 - Et*(temp2 - temp3)/dt           &
                                - Ro*(temp2*temp4 - temp1*temp5)

                        ! v (interieur)    

                        temp3 = (leta(i,j+1) - leta(i,j-1))/2/dy
                        temp5 = (eta(i,j+1) + eta(i,j-1) - 2*eta(i,j))/(dy*dy)
                        v2(i,j) = temp2 - Et*(temp1 - temp3)/dt            &
                                - Ro*(temp2*temp5 - temp1*temp4)
                    end do

                    

                    ! u (j=1)            
                    j = 1

                    temp1 = (4*eta(i,j+1)  -eta(i,j+2) - 3*eta(i,j))/2/dy
                    temp2 = (eta(i+1,j) - eta(i-1,j))/2/dx
                    temp3 = (leta(i+1,j) - leta(i-1,j))/2/dx
                    d1 = temp2
                    d2 = (eta(i+1,j+1) - eta(i-1,j+1))/(2*dx)
                    d3 = (eta(i+1,j+2) - eta(i-1,j+2))/(2*dx)
                    temp4 = (4*d2 - d3 - 3*d1)/(2*dy)              
                    temp5 = (eta(i+1,j) + eta(i-1,j) - 2*eta(i,j))/(dx*dx)
                    v1(i,j) = -temp1 - Et*(temp2-temp3)/dt            &
                              -Ro*(temp2*temp4 - temp1*temp5)

                    ! v (j=1)      

                    temp3 = (4*leta(i,j+1) - leta(i,j+2) - 3*leta(i,j))/2/dy 
                    temp5 = (eta(i,j+2) + eta(i,j) - 2*eta(i,j+1))/(dy*dy)
                    v2(i,j) = temp2-Et*(temp1 - temp3)/dt             &
                             - Ro*(temp2*temp5 - temp1*temp4)
                    
           
                    ! u (j=pasy)            
                    j = pasy

                    temp1 = (-4*eta(i,j-1) + eta(i,j-2) + 3*eta(i,j))/2/dy
                    temp2 = (eta(i+1,j) - eta(i-1,j))/2/dx
                    temp3 = (leta(i+1,j) - leta(i-1,j))/2/dx
                    d1 = temp2
                    d2 = (eta(i+1,j-1) - eta(i-1,j-1))/(2*dx)
                    d3 = (eta(i+1,j-2) - eta(i-1,j-2))/(2*dx)
                    temp4 = (-4*d2 + d3 + 3*d1)/(2*dy)   
                    temp5 = (eta(i+1,j) + eta(i-1,j) - 2*eta(i,j))/(dx*dx)
                    v1(i,j) = -temp1 - Et*(temp2 - temp3)/dt            &
                               - Ro*(temp2*temp4 - temp1*temp5)

                    ! v (j=pasy)     

                    temp3 = (-4*leta(i,j-1) + leta(i,j-2) + 3*leta(i,j))/2/dy 
                    temp5 = (eta(i,j-2) + eta(i,j) - 2*eta(i,j-1))/(dy*dy)
                    v2(i,j) = temp2 - Et*(temp1 - temp3)/dt         &
                             - Ro*(temp2*temp5 - temp1*temp4)
                end do

                ! périodicité

                do j = 1, pasy
                    v1(pasx+1,j) = v1(2,j)
                    v2(pasx+1,j) = v2(2,j)
                end do
            endif
        endif

        ! Resultats (save -> Matlab)

        if(n == nsave*fsave) then     
            do i = 2, pasx+1
                do j = 1, pasy
                    write(1,*) 'a(', nsave*pasx+i-1, ',', j, ')=', eta(i,j), ';'
                    if(saveu == 1) then
                        write(1,*) 'u(', nsave*pasx+i-1, ',', j, ')=', v1(i,j), ';'
                        write(1,*) 'v(', nsave*pasx+i-1, ',', j, ')=', v2(i,j), ';'
                    endif
                end do
            end do
            nsave = nsave + 1
        endif

    end do

    close (UNIT=1)

    !  Commentaires finaux

    print *, 'Calcul effectué sur ', Nmax*T*dt, 'sec.'
    print *, 'Nb itérations (moy) ', kmoy/Nmax
    print *, 'Résultats dans RES.M'
      
    ! free memory
    deallocate( x )
    deallocate( y )

    deallocate( eta )
    deallocate( b )
    deallocate( q )

    deallocate( jacob )
    deallocate( deta )

    deallocate( leta )
    deallocate( v1 )
    deallocate( v2 )
   
end program NIHOUL
