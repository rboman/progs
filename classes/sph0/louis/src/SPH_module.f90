!>SPH_module
!! @n Contains all the classes necessary to run a SPH simulation.
!! @brief Group of classes (types) and procedures definitions.
!! @author Louis Goffin
!! @date 2013-05-26
!! @version 1.0.0

module SPH_module
    
    implicit none
    
    integer, parameter :: DP  = KIND(1.0D0)            !< double precision
    !integer, parameter :: DP  = KIND(1.0)             !< single precision
    real(DP), parameter :: pi = 3.141592653589793238462643383279502884197_dp   !< \f$ \pi \f$
    
    
    !> Minimal link class
    !! @n This class is a minimal pointer, i.e. the object is composed of only a pointer.
    !!    No other information is included. The pointers point toward particles    
    type min_link
        class(fixed_particle), pointer :: ptr => null()     !< pointer toward a particle
    end type min_link
    
    
    !> Link class
    !! @n This class contains a pointer that points toward an object
    !! and the distance between 2 particles. This class is used to
    !! build vectors of pointers toward objects.
    
    type link
        class(fixed_particle), pointer :: ptr => null()     !< pointer toward a particle
        real(DP) :: r = 0.0_dp                       !< distance between neighbours
    end type link
    
    
    !> List class
    !! @n This class is a list that contains pointers toward objects (+distance).
    !! The only problem of this list class is that only link objects (ptr+r) can be added
    
    type list
        integer :: nbr     = 0                          !< number of elements in the list
        integer :: max_nbr = 0                          !< maxnumber of elements in the list (size of the array)
        integer :: incr    = 35                         !< increment: number of spaces to add when the list is full
        type(link), dimension(:), allocatable :: lst    !< list containing elements
    
        contains
            procedure :: initList   => list_initList
            procedure :: addElement => list_addElement
            procedure :: resetList  => list_resetList
    end type list


    !> Fixed particle class
    !! @n This class contains a certain number of parameters describing
    !! the state of a fixed particle (boundary particle).It also
    !! includes the needed procedures to calculate the continuity
    !! and some other equations.
    
    type fixed_particle
        real(DP), dimension(1:3, 1:3) :: coord       !< 3x3 array containing the coordinates of a particle.
                                                     !!      column 1 = currentTime
                                                     !!      column 2 = RKstep
                                                     !!      column 3 = nextTime
        real(DP), dimension(1:3, 1:3) :: speed       !< 3x3 array containing the velocity of a particle.
                                                     !!      column 1 = currentTime
                                                     !!      column 2 = RKstep
                                                     !!      column 3 = nextTime
        real(DP), dimension(1:3) :: rho              !< 3x1 array containing the density of a particle.
                                                     !!      element 1 = currentTime
                                                     !!      element 2 = RKstep
                                                     !!      element 3 = nextTime
        real(DP) :: m                                !< mass of the particle
        real(DP), dimension(1:3) :: p                !< pressure of the particle
                                                     !!      element 1 = currentTime
                                                     !!      element 2 = RKstep
                                                     !!      element 3 = nextTime
        real(DP), dimension(1:3) :: c                !< speed of sound of a particle
                                                     !!      element 1 = currentTime
                                                     !!      element 2 = RKstep
                                                     !!      element 3 = nextTime
        real(DP) :: h                                !< smoothing length
        type(list) :: neighbours                     !< list of neighbours
        integer :: numOfNeighbours                   !< number of neighbours
        real(DP), dimension(1:3, 1:150) :: vec_gradW !< array that contains the gradient for every
                                                     !! neighbours; initially set to 150 elements to
                                                     !! increase the computational efficiency
        real(DP), dimension(1:3, 1:150):: vec_gradW_mod !< corrected vec_gradW if asked
        class(particle_manager), pointer :: manager     !< pointer toward the object particle_manager
        real(DP) :: max_mu_ab                        !< maximum mu_ab of a particle (used for the timestep calculation)
        
        contains
            procedure :: save2disk => particle_save2disk
            procedure :: loadfromdisk => particle_loadfromdisk
            procedure :: getNeighbours
            procedure :: calcPressure
            procedure :: calcCelerity
            procedure :: gradW
            procedure :: kernel_corr
            procedure :: varUpdate => varUpdate_fixed
    end type fixed_particle
    
    
    !> Mobile_particle class
    !! @n This is an extension of the fixed_particle class.
    !! The procedure varUpdtate is overwritten to include the update of u and x.
    
    type, extends(fixed_particle) :: mobile_particle
        contains
            procedure :: varUpdate  =>  varUpdate_mobile
            procedure :: ArtificialViscosity
    end type mobile_particle
    
    
    !> Particle_sort class
    !! @n This class is able to sort the particles. A grid is generated
    !! and the particles are sorted in each cell.
    type particle_sort
        real(DP) :: h_max                                 !< maximum smoothing length
        real(DP) :: cellSize                              !< length of a side of a cube
        integer  :: nCells = 0                            !< number of cells in the domain
        integer  :: nCellsSide                            !< number of cells on a row
        logical  :: init                                  !< true if the cells must be initialised
        type(list), dimension(:), allocatable :: storage  !< vector of lists that contain
                                                          !! the particles in a cell
        class(particle_manager), pointer :: manager       !< pointer toward the object particle_manager
        
        contains
            procedure :: get_h_max
            procedure :: setCells
            procedure :: particlesSort
    end type particle_sort
    
    !> "enums"
    
    integer, parameter :: K_CUBIC_SPLINE   = 1
    integer, parameter :: K_QUADRATIC      = 2
    integer, parameter :: K_QUINTIC_SPLINE = 3
    
    integer, parameter :: KCORR_OFF = 0
    integer, parameter :: KCORR_ON  = 1

    integer, parameter :: LAW_IDEAL_GAS  = 1
    integer, parameter :: LAW_QINC_FLUID = 2

    
    !> Particle_manager class
    !! @n This class is used to manage all the particles,
    !! i.e. it contains a reference to every particles,
    !! it contains a number of parameters useful for the problem
    !! (variable smoothing length or not, ...), it has a solver, etc.
    
    type particle_manager
        type(particle_sort) :: sorting                     !< sorting machine
        type(min_link), dimension(:), allocatable :: part  !< array of pointers toward particles
        
        integer  :: numFP              !< number of fixed particles
        integer  :: numMP              !< number of mobile particles
        integer  :: numPart            !< number of particles (FP+MP)
        integer  :: kernelKind         !< kind of kernel
        integer  :: kappa              !< kappa linked to the eqn state
        real(DP) :: alpha              !< weighting factor in the artificial viscosity formulation
        real(DP) :: beta               !< weighting factor in the artificial viscosity formulation
        integer  :: eqnState           !< equation of state
                                       !!   1 = ideal gas law
                                       !!   2 = quasi-incompressible fluid
        integer  :: state_gamma        !< power in eqn State 2.
                                       !! often taken around 7
        real(DP) :: molMass            !< Molar mass of the fluid for the prefect gas law
    
        integer  :: kernelCorrection   !< correction of the kernel
                                       !!   0 = no correction
                                       !!   1 = correction enabled
    
        real(DP) :: maxTime            !< simulation time in seconds
        real(DP) :: saveInt            !< saving interval
        real(DP) :: h_0                !< initial smoothing length
        real(DP) :: rho_0              !< density of the fluid at free surface
        real(DP) :: c_0                !< speed of sound in normal conditions
        real(DP) :: timeStep           !< timestep (not constant)
        real(DP) :: currentTime        !< current time
        integer  :: RKstep             !< used to know in which RK iteration we are      [RB] (1 or 2)
    
        real(DP) :: dom_dim            !< length of a side of the domain
                                       !! (exterior particle to exterior particle).
                                       !! the domain is assumed to be cubic
    
        contains
            procedure :: readPRM
            procedure :: initialisation
            procedure :: solver
            procedure :: timeStepUpdate
            procedure :: slUpdate
            procedure :: savePartSet
    end type particle_manager


    contains

    !> calculates the distance between two particles
    function eval_r(xyz, xyz2)
        real(DP), dimension(3) :: xyz
        real(DP), dimension(3) :: xyz2
        real(DP) :: eval_r
    
        eval_r = sqrt(sum( (xyz(:)-xyz2(:))*(xyz(:)-xyz2(:)) ))
    end function eval_r
    
    ! ------------------------------------------------------------------------
    ! list
    ! ------------------------------------------------------------------------
    
    !> list/initList : initialise a list when it is first created
    subroutine list_initList(this)
        class(list) :: this

        if(this%incr < 1) then  ! [RB] me semble inutile (incr=35 et ne varie pas)
            this%incr = 1
        end if
        this%max_nbr = this%incr
        allocate( this%lst(1:this%max_nbr) )
    end subroutine list_initList
    
    
    !> list/addElement : adds an element to a list. 
    !!           If the list is full, then it increases the size of the list.
    !! @param ptr  : pointer of type link to be added in the list
    !! @param r    : distance between two particles
    
    subroutine list_addElement(this, ptr, r)
        class(list) :: this
        
        class(fixed_particle), pointer :: ptr
        real(DP) :: r
        type(link), dimension(:), allocatable :: temp_lst   !< temporary list used when it is necessary
                                                            !! to increase the size of the existing list
        
        ! if the list is empty, it must be initialised
        if(this%max_nbr == 0) then
            call this%initList
            
        ! if the list is full it must be resized
        elseif(this%nbr == this%max_nbr) then
            allocate(temp_lst(1:this%max_nbr+this%incr))
            temp_lst(1:this%max_nbr) = this%lst(1:this%max_nbr)
            call move_alloc(temp_lst, this%lst)     ! [RB] c'est une fct fortran
            this%max_nbr = this%max_nbr + this%incr
        end if
        this%nbr = this%nbr + 1
        this%lst(this%nbr)%ptr => ptr
        this%lst(this%nbr)%r = r
    end subroutine list_addElement


    !> list/resetList : resets the list. 
    !!      It only sets the number of elements to 0 
    !!      but it keeps the maximal size to max_nbr
    
    subroutine list_resetList(this)
        class(list) this
        this%nbr = 0
    end subroutine list_resetList

    ! ------------------------------------------------------------------------
    ! fixed_particle
    ! ------------------------------------------------------------------------
    
    subroutine particle_save2disk(this, ufile)
        class(fixed_particle), target :: this 
        integer, intent(in) :: ufile !< unit
        
        write(unit=ufile, fmt="(E15.7,E15.7,E15.7,E15.7,E15.7,E15.7,E15.7,E15.7,E15.7,E15.7,E15.7,E15.7, I8)")   &
        this%coord(1, 1),  &
        this%coord(2, 1),  &
        this%coord(3, 1),  &
        this%speed(1, 1),  &
        this%speed(2, 1),  &
        this%speed(3, 1),  &
        this%rho(1),       &
        this%p(1),         &
        this%m,            &
        this%c(1),         &
        this%h,            &
        this%max_mu_ab,    &
        this%numOfNeighbours
    end subroutine particle_save2disk
    
    
   subroutine particle_loadfromdisk(this, ufile, h_0)
        class(fixed_particle), target :: this 
        integer, intent(in) :: ufile !< unit
        real(DP) :: x, y, z, u_x, u_y, u_z, rho, m
        real(DP) :: h_0
        
        read(ufile, *) x, y, z, u_x, u_y, u_z, rho, m
        this%coord = 0.d0
        this%coord(1, 1) = x
        this%coord(2, 1) = y
        this%coord(3, 1) = z
        this%speed = 0.d0
        this%speed(1, 1) = u_x
        this%speed(2, 1) = u_y
        this%speed(3, 1) = u_z
        this%rho = 0.d0
        this%rho(1) = rho
        this%m = m
        this%h = h_0
        this%p = 0.d0
        this%p(1) = this%calcPressure(this%rho(1))
        this%c = 0.d0
        this%c(1) = this%calcCelerity(this%rho(1))
    end subroutine particle_loadfromdisk
    
    !> fixed_particle/getNeighbours is a routine that questions the "particle_sort" object
    !!               to get the particles in the neighbouring cells. The distance is
    !!               calculated and the neighbours selected.

    subroutine getNeighbours(this)
        class(fixed_particle), target :: this
        
        real(DP), dimension(:), pointer, contiguous :: xyz       !< position of the particle
        integer :: xCell, yCell, zCell                           !< number of the cell according to x, y and z
        integer :: nCellsSide                                    !< number of cells on a row of the domain
        integer :: i, j, k                                       !< loop counters
        integer, dimension(1:27) :: cellsToCheck                 !< number of the cells to check for the neighbours
        real(DP), dimension(:), pointer, contiguous :: neighXYZ  !< coordinates of a neighbour
    
        real(DP) :: r                                !< distance between two particles
        class(fixed_particle), pointer :: cur_ptr    !< current pointer toward a particle
        type(link), pointer :: cur_neigh             !< current list of neighbours
        type(particle_sort), pointer :: srt          !< pointer toward the sorting machine
        type(list), pointer :: storage               !< pointer toward the storage
        
        integer :: cur_RKstep               !<  RKstep
        
        ! [RB] --
        integer :: twice
        
        ! pointer initialisation
        srt => this%manager%sorting          ! [RB] chaque particule pointe vers le manager
        cur_RKstep = this%manager%RKstep
        
        ! coordinates of the particle
        xyz => this%coord(:, cur_RKstep)
    
        if(cur_RKstep == 1) then
        
            ! calculates the number of the cell in which the particle is
            nCellsSide = srt%nCellsSide
            
            xCell = nint( (xyz(1)-mod(xyz(1), srt%cellSize)) / srt%cellSize ) + 1
            yCell = nint( (xyz(2)-mod(xyz(2), srt%cellSize)) / srt%cellSize ) + 1
            zCell = nint( (xyz(3)-mod(xyz(3), srt%cellSize)) / srt%cellSize ) + 1
            
            if(xCell<1) then
                xCell = 1
            end if
            if(xCell>nCellsSide) then
                xCell = nCellsSide
            end if 
            if(yCell<1) then
                yCell = 1
            end if
            if(yCell>nCellsSide) then
                yCell = nCellsSide
            end if 
            if(zCell<1) then
                zCell = 1
            end if
            if(zCell>nCellsSide) then
                zCell = nCellsSide
            end if
            
            ! calculates the number of the neighbouring cells
            do i = -1, 1
                do j = -1, 1
                    do k = -1, 1
                        if((xCell+i>0).and.(yCell+j>0).and.(zCell+k>0).and. &
                           (xCell+i <= nCellsSide).and. &
                           (yCell+j <= nCellsSide).and. &
                           (zCell+k <= nCellsSide)        ) then
                           cellsToCheck( (i+1)*9 + (j+1)*3 + (k+2) ) = &
                                (xCell+i-1)*nCellsSide**2 + (yCell+j-1)*nCellsSide + (zCell+k)
                        else
                            cellsToCheck((i+1)*9 + (j+1)*3 + (k+2)) = 0
                        end if
                    end do
                end do
            end do
        end if
        
        
        ! stores the neighbours of the particle in the neighbours list.
        ! First, the list is reset, then the neighbouring cells are scanned.
        ! In each cell, the distance between the two particles is calculated.
        ! If it is lower than the support domain and it is not the particle we
        ! are working with (r>0 but here r>1E-12 for numerical errors), an  element
        ! link(ptr+r) is added to the neighbours list.
        ! For the second RK step, only the distances r are recalculated. It is assumed that
        ! the neighbours remain the same between 2 RK step.
        
        if(cur_RKstep == 1) then
            call this%neighbours%resetList()
            twice = 0 ! [RB]
            do i = 1, 27
                if(cellsToCheck(i)>0) then
                    storage => srt%storage(cellsToCheck(i))
                    do j = 1, srt%storage(cellsToCheck(i))%nbr
                        cur_ptr => storage%lst(j)%ptr
                        neighXYZ => cur_ptr%coord(:, cur_RKstep)
                        r = eval_r(xyz, neighXYZ)
                        if(r<= this%manager%kappa*this%h) then
                            if(r>1E-12) then   ! [RB] why not cur_ptr /= this?
                                call this%neighbours%addElement(cur_ptr, r)
                            else
                                twice = twice + 1 ! [RB]
                            end if
                        end if
                    end do
                end if
            end do
            ! [RB] safeguard
            if (twice.ne. 1) then
                print *, 'safeguard activated!'
                print *, '    one particle has been taken into account', twice, ' times'
                print *, '    xCell =', xCell
                print *, '    yCell =', yCell
                print *, '    zCell =', zCell
                print *, ' cellsToCheck =', cellsToCheck
                stop
            end if
            this%numOfNeighbours = this%neighbours%nbr
        else
            ! RK step2 - same neighbours and r is updated
            do i = 1, this%numOfNeighbours
                cur_neigh => this%neighbours%lst(i)
                neighXYZ  => cur_neigh%ptr%coord(:, cur_RKstep)
                cur_neigh%r = eval_r(xyz, neighXYZ)
            end do
        end if
    end subroutine getNeighbours


    !> fixed_particle/calcPressure is a function that calculates the pressure according
    !!             to the equation of state chosen.
    !! @param rho  : actual density
    
    function calcPressure(this, rho)
        class(fixed_particle) this
        
        real(DP) :: rho
        real(DP) :: calcPressure
        real(DP), parameter :: idealGasCst = 8.3144621d0
        real(DP) :: B

        select case(this%manager%eqnState)
        case ( LAW_IDEAL_GAS)       
            calcPressure = (rho/this%manager%rho_0-1) &
                       * idealGasCst*293.15d0/this%manager%molMass  ! eq (3.24)
        case ( LAW_QINC_FLUID )  
            B = this%manager%c_0**2 &
              * this%manager%rho_0 / this%manager%state_gamma  ! eq (3.27)
            calcPressure = B*((rho/this%manager%rho_0)**this%manager%state_gamma - 1)   ! eq (3.25)
        case default
            print *, 'Bad Equ of state (1,2)'
            stop
        end select
    end function calcPressure
    
    
    !> fixed_particle/calcCelerity : calculates the celerity according
    !!              to the equation of state chosen. 
    !!              The equation used is @f[c = \sqrt{\frac{dp}{d\rho}}@f]
    !! @param rho  : actual density
    
    function calcCelerity(this, rho)
        class(fixed_particle) :: this
        
        real(DP), intent(in)   :: rho
        real(DP)  :: calcCelerity
        
        select case(this%manager%eqnState)
        case( LAW_IDEAL_GAS )        
            ! 1 = considering the ideal gas law at 20 degrees C
            calcCelerity = this%manager%c_0                                       ! eq (3.36)
        case ( LAW_QINC_FLUID )    
            ! 2 = considering a quasi-incompressible fluid
            calcCelerity = this%manager%c_0 &
                * sqrt((rho/this%manager%rho_0)**(this%manager%state_gamma-1))  ! eq (3.37)
        case default
            print *, 'Bad Equ of state (1,2)'
            stop                
        end select
    end function calcCelerity
    
    
    !> gradW : creates a vector that contains the values
    !!       of the gradient for each neighbour.
    
    subroutine gradW(this)
        class(fixed_particle), target :: this
        integer  :: i
        real(DP) :: alpha_d             !< normalisation coefficient
        real(DP) :: r                   !< distance between a particle and a neighbour
        class(fixed_particle), pointer :: cur_neigh     !< pointer toward a neighbour
        real(DP) :: cur_h               !< value of h
        integer  :: cur_RKstep          !< pointer toward the current RK step
        
        if(this%numOfNeighbours>150) then
            print *, 'Error: Number of neighbours greater than expected (max 150 for vec_gradW): ', this%numOfNeighbours
            stop
        end if
        
        cur_h = this%h
        cur_RKstep = this%manager%RKstep   
        
        select case(this%manager%kernelKind)
        case ( K_CUBIC_SPLINE) 
        
            alpha_d = 3.d0/(2.d0*pi*cur_h**3)    ! [RB] efficiency of x**3.0d0 vs x**3 vs x*x*x ??
                                                    ! values of alpha_d in table 2.1 p 23
            do i = 1, this%numOfNeighbours
                r = this%neighbours%lst(i)%r       ! [RB] a pointer is useless here!
                cur_neigh => this%neighbours%lst(i)%ptr
                if((r/cur_h>= 0.d0).and.(r/cur_h<1.d0)) then ! [RB] could "r/cur_h" be negative??
                    this%vec_gradW(:, i) = alpha_d/cur_h &
                                * (3.d0/2.d0*(r/cur_h)**2 - 2.d0*(r/cur_h) ) &
                                * (this%coord(:, cur_RKstep) - cur_neigh%coord(:, cur_RKstep))/r       ! eq (2.26)
                else if((r/cur_h>= 1.d0).and.(r/cur_h<2.d0)) then
                    this%vec_gradW(:, i) = alpha_d/cur_h     &
                                * (-0.5d0*(2.d0-r/cur_h)**2) &
                                * (this%coord(:, cur_RKstep) - cur_neigh%coord(:, cur_RKstep))/r
                else
                    this%vec_gradW(:, i) = 0.d0
                end if
            end do
            
        case ( K_QUADRATIC )
        
            alpha_d = 5.d0/(4.d0*pi*cur_h**3)
            
            do i = 1, this%numOfNeighbours
                r = this%neighbours%lst(i)%r  ! [RB] a pointer is useless here!
                cur_neigh => this%neighbours%lst(i)%ptr
                
                if((r/cur_h>= 0.d0).and.(r/cur_h<= 2.d0)) then  ! [RB] could "r/cur_h" be negative??
                    this%vec_gradW(:, i) = alpha_d/cur_h &
                                * (3.d0/8.d0*r/cur_h-3.d0/4.d0)   &
                                * (this%coord(:, cur_RKstep)-cur_neigh%coord(:, cur_RKstep))/r            ! eq (2.28)
                else
                    this%vec_gradW(:, i) = 0.d0
                end if
            end do
            
        case ( K_QUINTIC_SPLINE)
        
            alpha_d = 3.d0/(359.d0*pi*cur_h**3)
            
            do i = 1, this%numOfNeighbours
                r = this%neighbours%lst(i)%r  ! [RB] a pointer is useless here!
                cur_neigh => this%neighbours%lst(i)%ptr
                if((r/cur_h>= 0.d0).and.(r/cur_h<1.d0)) then
                    this%vec_gradW(:, i) = alpha_d/cur_h         &
                                * ( -5.d0*(3.d0-r/cur_h)**4      &
                                    + 30.d0*(2.d0-r/cur_h)**4    &
                                    - 75.d0*(1.d0-r/cur_h)**4 )  &
                                * (this%coord(:, cur_RKstep)-cur_neigh%coord(:, cur_RKstep))/r      ! eq (2.32)
                elseif((r/cur_h>= 1.d0).and.(r/cur_h<2.d0)) then
                    this%vec_gradW(:, i) = alpha_d/cur_h         &
                                * ( -5.d0*(3.d0-r/cur_h)**4      &
                                    +30.d0*(2.d0-r/cur_h)**4 )   &
                                * (this%coord(:, cur_RKstep)-cur_neigh%coord(:, cur_RKstep))/r
                elseif((r/cur_h>= 2.d0).and.(r/cur_h<3.d0)) then
                    this%vec_gradW(:, i) = alpha_d/cur_h         &
                                * (-5.d0*(3.d0-r/cur_h)**4)      &
                                * (this%coord(:, cur_RKstep)-cur_neigh%coord(:, cur_RKstep))/r
                else
                    this%vec_gradW(:, i) = 0.d0
                end if
            end do
            
        case default
            print *, 'Bad value for kernel kind (1,2,3)'
            stop
        end select
        
        if(this%manager%kernelCorrection == KCORR_ON) then
            this%vec_gradW_mod = this%vec_gradW
        end if
    end subroutine gradW
    
    
    !> kernel_corr is a routine that takes into account the fact that the kernel may be truncated.
    !!             It corrects the gradient of the kernel
    
    subroutine kernel_corr(this)
        class(fixed_particle) :: this
        real(DP), dimension(3, 3) :: M           !< matrix used to correct the kernel gradient
        real(DP), dimension(3, 3) :: L           !< inverse of the matrix used to correct the kernel gradient
        real(DP) :: detM                         !< determinant of M
        integer  :: i                            !< loop counter
        real(DP) :: MDivRho                      !< m_b/rho_b
        class(fixed_particle), pointer :: cur_neigh     !< pointer toward the current neighbour
        integer :: cur_RKstep           !< current RK step
        
        cur_RKstep = this%manager%RKstep
        M(1, 1) = 0.d0
        M(2, 2) = 0.d0
        M(3, 3) = 0.d0
        M(1, 2) = 0.d0
        M(1, 3) = 0.d0
        M(2, 3) = 0.d0
        
        do i = 1, this%numOfNeighbours
            cur_neigh => this%neighbours%lst(i)%ptr
            MDivRho = cur_neigh%m/cur_neigh%rho(cur_RKstep)
            M(1, 1) = M(1, 1) + MDivRho * (cur_neigh%coord(1, cur_RKstep) - this%coord(1, cur_RKstep)) * this%vec_gradW(1, i)
            M(2, 2) = M(2, 2) + MDivRho * (cur_neigh%coord(2, cur_RKstep) - this%coord(2, cur_RKstep)) * this%vec_gradW(2, i)
            M(3, 3) = M(3, 3) + MDivRho * (cur_neigh%coord(3, cur_RKstep) - this%coord(3, cur_RKstep)) * this%vec_gradW(3, i)
            M(1, 2) = M(1, 2) + MDivRho * (cur_neigh%coord(1, cur_RKstep) - this%coord(1, cur_RKstep)) * this%vec_gradW(2, i)
            M(1, 3) = M(1, 3) + MDivRho * (cur_neigh%coord(1, cur_RKstep) - this%coord(1, cur_RKstep)) * this%vec_gradW(3, i)
            M(2, 3) = M(2, 3) + MDivRho * (cur_neigh%coord(2, cur_RKstep) - this%coord(2, cur_RKstep)) * this%vec_gradW(3, i)
        end do
        M(2, 1) = M(1, 2)       !< M is symmetric
        M(3, 1) = M(1, 3)       !< M is symmetric
        M(3, 2) = M(2, 3)       !< M is symmetric
    
        detM =   M(1, 1) * ( M(2, 2)*M(3, 3)-M(3, 2)*M(2, 3) ) &
               - M(1, 2) * ( M(2, 1)*M(3, 3)-M(3, 1)*M(2, 3) ) &
               + M(1, 3) * ( M(2, 1)*M(3, 2)-M(3, 1)*M(2, 2) )
        if(detM.eq.0.0) then       
            print *, "detM==0!"
            stop
        end if
               
        L(1, 1) = M(2, 2)*M(3, 3) - M(3, 2)*M(2, 3)
        L(2, 2) = M(1, 1)*M(3, 3) - M(3, 1)*M(1, 3)
        L(3, 3) = M(1, 1)*M(2, 2) - M(2, 1)*M(1, 2)
        L(1, 2) = M(3, 1)*M(2, 3) - M(2, 1)*M(3, 3)
        L(2, 1) = L(1, 2)           ! the inverse of a symmetric matrix is symmetric
        L(1, 3) = M(2, 1)*M(3, 2) - M(3, 1)*M(2, 2)
    
        L(3, 1) = L(1, 3)           ! the inverse of a symmetric matrix is symmetric
        L(2, 3) = M(3, 1)*M(1, 2) - M(1, 1)*M(3, 2)
        L(3, 2) = L(2, 3)           ! the inverse of a symmetric matrix is symmetric
        L = (1.d0/detM)*L
        
        do i = 1, this%numOfNeighbours
            !this%vec_gradW_mod(1, i) = L(1, 1)*this%vec_gradW(1, i) + L(1, 2)*this%vec_gradW(1, i) + L(1, 3)*this%vec_gradW(3, i)
            this%vec_gradW_mod(1, i) = L(1, 1)*this%vec_gradW(1, i) + L(1, 2)*this%vec_gradW(2, i) + L(1, 3)*this%vec_gradW(3, i) ! [RB] 1=>2 BUG?
            this%vec_gradW_mod(2, i) = L(2, 1)*this%vec_gradW(1, i) + L(2, 2)*this%vec_gradW(2, i) + L(2, 3)*this%vec_gradW(3, i)
            this%vec_gradW_mod(3, i) = L(3, 1)*this%vec_gradW(1, i) + L(3, 2)*this%vec_gradW(2, i) + L(3, 3)*this%vec_gradW(3, i)
        end do
        
    end subroutine kernel_corr

    
    !> fixed_particle/varUpdate_fixed : update the density of a fixed particle.
    !!  The update of the velocity and the position are not performed.
    !!  The integration scheme is a RK22 scheme.
    
    subroutine varUpdate_fixed(this)
        class(fixed_particle) :: this
        real(DP) :: Delta_rho                        !< \f$d\rho/dt\f$
        real(DP), dimension(1:3) :: u_ab             !< relative velocity between the particle and a neighbour
        integer :: i                                 !< loop counter
        integer :: cur_RKstep               !< pointer toward the value of the current RK step
        class(fixed_particle), pointer :: cur_neigh  !< current neighbour
        real(DP) :: dt
        
        Delta_rho = 0.d0
        cur_RKstep = this%manager%RKstep
        
        call this%getNeighbours()
        call this%gradW()
    
        do i = 1, this%numOfNeighbours
            cur_neigh => this%neighbours%lst(i)%ptr
            u_ab(:) = this%speed(:, cur_RKstep) - cur_neigh%speed(:, cur_RKstep)
            Delta_rho = Delta_rho + this%m * dot_product(u_ab, this%vec_gradW(:, i))
        end do
        
        dt = this%manager%timeStep
        
        if(cur_RKstep == 1) then                    !< 1st RK step
            this%rho(2) = this%rho(1) + Delta_rho * dt
            this%rho(3) = this%rho(1) + Delta_rho * dt/2.d0
            this%speed(:, 2) = this%speed(:, 1)
            this%coord(:, 2) = this%coord(:, 1)
            this%p(2) = this%calcPressure(this%rho(2))
            this%c(2) = this%calcCelerity(this%rho(2))
        else                                        !< 2nd RK step
            this%rho(3) = this%rho(3) + Delta_rho * dt/2.d0
            this%speed(:, 3) = this%speed(:, 2)
            this%coord(:, 3) = this%coord(:, 2)
            this%p(3) = this%calcPressure(this%rho(3))
            this%c(3) = this%calcCelerity(this%rho(3))
        end if
    end subroutine varUpdate_fixed
    
    ! ------------------------------------------------------------------------
    ! mobile_particle
    ! ------------------------------------------------------------------------
      
    !> mobile_particle/varUpdate_mobile is a routine that updates the density, the velocity
    !!                  and the position of a mobile particle.
    !!                  The integration scheme is a RK22 scheme.
    
    subroutine varUpdate_mobile(this)
        class(mobile_particle) :: this
        real(DP) :: Delta_rho                    !< \f$d\rho/dt\f$
        real(DP), dimension(1:3) :: Delta_u      !< \f$du/dt\f$
        real(DP), dimension(1:3) :: Delta_x      !< \f$dx/dt\f$
        real(DP), dimension(1:3) :: u_ab         !< relative velocity between the particle and a neighbour
        real(DP), dimension(1:3) :: F            !< Volume forces
        real(DP) :: pi_ab                        !< Artificial viscosity
        integer :: i                                    !< loopcounter
        integer :: cur_RKstep                  !< pointer toward the value of the current RK step
        class(fixed_particle), pointer :: cur_neigh     !< current neighbour
        real(DP) :: dt
        
        Delta_rho = 0.d0
        Delta_u = (/ 0.d0, 0.d0, 0.d0 /)
        Delta_x = (/ 0.d0, 0.d0, 0.d0 /)
        F = (/ real(0, DP), real(0, DP), real(-9.81, DP) /)
        
        cur_RKstep = this%manager%RKstep
        
        call this%getNeighbours()
        call this%gradW()
        
        if(this%manager%kernelCorrection == KCORR_ON) then
            call this%kernel_corr()
        end if
        
        ! reset max_mu_ab
        if(cur_RKstep == 1) then
            this%max_mu_ab = 0.d0
        end if
    
        
        
        select case(this%manager%kernelCorrection)
        case( KCORR_ON )
            do i = 1, this%numOfNeighbours
                cur_neigh => this%neighbours%lst(i)%ptr
                u_ab(:)   = this%speed(:, cur_RKstep) - cur_neigh%speed(:, cur_RKstep)
                pi_ab     = this%ArtificialViscosity(cur_neigh, this%manager%alpha, this%manager%beta)
                Delta_rho = Delta_rho &
                            + this%m * dot_product(u_ab, this%vec_gradW(:, i))
                Delta_u   = Delta_u   &
                            + this%m * (cur_neigh%p(cur_RKstep) / cur_neigh%rho(cur_RKstep)**2 &
                                        + this%p(cur_RKstep) / this%rho(cur_RKstep)**2 &
                                        + pi_ab ) &
                              * this%vec_gradW_mod(:, i)   ! <= difference here
            end do
        case ( KCORR_OFF )
            do i = 1, this%numOfNeighbours
                cur_neigh => this%neighbours%lst(i)%ptr
                u_ab(:)   = this%speed(:, cur_RKstep) - cur_neigh%speed(:, cur_RKstep)
                pi_ab     = this%ArtificialViscosity(cur_neigh, this%manager%alpha, this%manager%beta)
                Delta_rho = Delta_rho &
                            + this%m * dot_product(u_ab, this%vec_gradW(:, i))
                Delta_u   = Delta_u &
                            + this%m * (cur_neigh%p(cur_RKstep)/cur_neigh%rho(cur_RKstep)**2 &
                                        + this%p(cur_RKstep) / this%rho(cur_RKstep)**2 &
                                        + pi_ab ) &
                              * this%vec_gradW(:, i)      ! <= difference here
            end do
        case default
            print *,'Bad value of kernel correction'
            stop
        end select
        
        Delta_u = -Delta_u + F
        dt = this%manager%timeStep
         
        if(cur_RKstep == 1) then                ! 1st RK step
            this%rho(2) = this%rho(1) + Delta_rho * dt
            this%rho(3) = this%rho(1) + Delta_rho * dt/2.d0
            this%speed(:, 2) = this%speed(:, 1) + Delta_u * dt
            this%speed(:, 3) = this%speed(:, 1) + Delta_u * dt/2.d0
            this%coord(:, 2) = this%coord(:, 1) + this%speed(:, 1) * dt
            this%coord(:, 3) = this%coord(:, 1) + this%speed(:, 1) * dt/2.d0
            this%p(2) = this%calcPressure(this%rho(2))
            this%c(2) = this%calcCelerity(this%rho(2))
        else                                    ! 2nd RK step
            this%rho(3) = this%rho(3) + Delta_rho * dt/2.d0
            this%speed(:, 3) = this%speed(:, 3) + Delta_u * dt/2.d0
            this%coord(:, 3) = this%coord(:, 3) + this%speed(:, 2) * dt/2.d0
            this%p(3) = this%calcPressure(this%rho(3))
            this%c(3) = this%calcCelerity(this%rho(3))
        end if
    end subroutine varUpdate_mobile

    
    !> mobile_particle/ArtificialViscosity calculates the viscosity term
    !!      in the momentum equation.
    !! @param neighObj : neighbouring object
    !! @param alpha    : coefficicent in the artificial viscosity formulation
    !! @param beta     : coefficicent in the artificial viscosity formulation
    
    function ArtificialViscosity(this, neighObj, alpha, beta)
        class(mobile_particle) :: this
        class(fixed_particle) :: neighObj
        real(DP) :: alpha
        real(DP) :: beta
        real(DP) :: ArtificialViscosity
        real(DP) :: mu_ab = 0.d0                 !< this term represents a kind of viscosity
        real(DP), dimension(1:3) :: u_ab         !< relative velocity of a in comparison with b
        real(DP), dimension(1:3) :: x_ab         !< the distance between a and b
        real(DP) :: c_ab                         !< mean speed of sound
        real(DP) :: rho_ab                       !< mean density
        
        u_ab = this%speed(:, this%manager%RKstep) - neighObj%speed(:, this%manager%RKstep)
        x_ab = this%coord(:, this%manager%RKstep) - neighObj%coord(:, this%manager%RKstep)
        
        if(dot_product(u_ab, x_ab)<0.0d0) then    
            ! mu_ab is calculated using \f[\mu_{ab} = \frac{h\vec{u}_{ab}\cdot\vec{x}_{ab}}{\vec{x}_{ab}^2+\eta^2}\f]
            mu_ab  = this%h * dot_product(u_ab, x_ab) / ( dot_product(x_ab, x_ab) + 0.01d0*this%h**2.d0 )
            c_ab   = 0.5d0 * (this%c(this%manager%RKstep) + neighObj%c(this%manager%RKstep))
            rho_ab = 0.5d0 * (this%rho(this%manager%RKstep) + neighObj%rho(this%manager%RKstep))
            
            ArtificialViscosity = (-alpha*c_ab*mu_ab + beta*mu_ab**2.d0)/rho_ab
        else
            ArtificialViscosity = 0.d0
        end if
        
        ! update of max_mu_ab for the calculation of the timestep
        if( (this%manager%RKstep == 1).and.(mu_ab>this%max_mu_ab) ) then
            this%max_mu_ab = mu_ab
        end if
    end function ArtificialViscosity

    ! ------------------------------------------------------------------------
    ! particle_manager
    ! ------------------------------------------------------------------------
    
    !> Reading and storing of the data in the parameter files
    subroutine readPRM(this, param_path)
        class(particle_manager), target :: this
        character(len=*) :: param_path
        
        open(unit = 1, file = trim(param_path))
        read(1, *) this%numFP
        read(1, *) this%numMP
        read(1, *) this%h_0
        read(1, *) this%c_0
        read(1, *) this%rho_0
        read(1, *) this%dom_dim
        read(1, *) this%kernelKind
        read(1, *) this%alpha
        read(1, *) this%beta
        read(1, *) this%eqnState
        read(1, *) this%state_gamma
        read(1, *) this%molMass
        read(1, *) this%kernelCorrection
        read(1, *) this%maxTime
        read(1, *) this%saveInt
        close(unit = 1)
    end subroutine readPRM
    
    
    !> particle_manager/initialisation : initialises the particle manager
    !!    and all the particles. The routine takes the data from external files.
    !!    The files paths are given in a file saved in the same directory 
    !!    as the program.
    !!    The external files contains: 
    !!      the parameters, 
    !!      the fixed particles properties and the mobile particles.
    
    subroutine initialisation(this)
        class(particle_manager), target :: this
        
        character(250) :: param_path            !< path of the parameters file
        character(250) :: fp_path               !< path of the fixed particle(fp) file
        character(250) :: mp_path               !< path of the mobile particle(mp) file
        
        integer :: i                            !< loop counter
        class(fixed_particle), pointer :: cur_ptr
    
        this%timeStep    = 1.0d-15              !< initial time step
        this%currentTime = 0.d0                 !< current time initialisation
        this%RKstep      = 1                    !< RK step counter initialisation
        
        ! Reading of the paths of the input files
        open(unit = 1, file = 'paths.txt')
        read(1, *) param_path
        read(1, *) fp_path
        read(1, *) mp_path
        close(unit = 1)
        
        call this%readPRM(param_path)

        ! allocation of the particles array
        this%numPart = this%numFP + this%numMP
        allocate(this%part(1:this%numPart))  
        
        select case(this%kernelKind)
        case( K_CUBIC_SPLINE )
            this%kappa = 2
        case( K_QUADRATIC )
            this%kappa = 2
        case( K_QUINTIC_SPLINE )
            this%kappa = 3
        end select
    
        ! Reading and storing of the data for the fixed particles
        open(unit = 1, file = trim(fp_path))
        do i = 1, this%numFP
            allocate(fixed_particle :: this%part(i)%ptr)
            cur_ptr => this%part(i)%ptr
            cur_ptr%manager => this
            call cur_ptr%loadfromdisk(1, this%h_0)           
            call cur_ptr%neighbours%initList()
        end do
        close(unit = 1)
        
        ! Reading and storing of the data for the mobile particles
        open(unit = 1, file = trim(mp_path))
        do i = 1, this%numMP  
            allocate(mobile_particle :: this%part(this%numFP+i)%ptr)
            cur_ptr => this%part(this%numFP+i)%ptr
            cur_ptr%manager => this
            call cur_ptr%loadfromdisk(1, this%h_0)           
            call cur_ptr%neighbours%initList()
        end do
        close(unit = 1)
        
        ! Particle sort
        this%sorting%manager => this
        this%sorting%init = .true.

        print *, 'Initialisation finished.'
    end subroutine initialisation

    
    !> save a particle set onto disk
    
    subroutine savePartSet(this, fname, ite, nbegin, nend)
        class(particle_manager) :: this
        character (len=*), intent(in) :: fname   !< file name prefix
        integer, intent(in) :: ite
        integer, intent(in) :: nbegin, nend
        
        integer :: i, ios
        character (len=250) :: filename
        
        write(filename , fmt="(A,'_',I0.8,'.res')") fname,ite
        open (unit=1, file=filename, action="write", iostat=ios)
        if ( ios /= 0 ) then
            print "('Cannot open result file')"
            stop
        end if
        do i = nbegin, nend
            call this%part(i)%ptr%save2disk(1)
        end do
        close(unit=1)
    end subroutine savePartSet
    
    
    !> particle_manager/solver: solves the problem. It loops over time and uses
    !!      a RK22 time integration scheme.
    
    subroutine solver(this)
        class(particle_manager) :: this
        
        integer :: i, j
        integer :: ite                  !< iteration counter
        logical :: to_save              !< saving flag. If true a saving is done
        class(fixed_particle), pointer :: p
        
        ite = 0
        
        do while(this%currentTime <= this%maxTime)
        
            ! Time increment and saving status
            if((floor(this%currentTime/this%saveInt) /= &
                floor((this%currentTime+this%timeStep)/this%saveInt)) .or. ite == 0) then
                to_save = .true.
            end if
            this%currentTime = this%currentTime + this%timeStep
            
            ! Runge-Kutta loop
            do j = 1, 2
                this%RKstep = j
                !if (j.eq.1) then ! [RB]
                    call this%sorting%particlesSort()
                !end if ! [RB]
                ! Loop over the particles
                !$OMP PARALLEL DO PRIVATE(i) SCHEDULE(DYNAMIC)
                do i = 1, this%numPart
                    call this%part(i)%ptr%varUpdate()
                end do
                !$OMP END PARALLEL DO
            end do
            
            ! Update of the current time variables (currentTime = nextTime)
            do i = 1, this%numPart
                p => this%part(i)%ptr
                p%rho(1)      = p%rho(3)
                p%p(1)        = p%p(3)
                p%c(1)        = p%c(3)
                p%speed(:, 1) = p%speed(:, 3)
                p%coord(:, 1) = p%coord(:, 3)
            end do
            
            ! Test for the data saving
            if(to_save) then
                call this%savePartSet('resMP', ite, this%numFP+1, this%numFP+this%numMP)
                call this%savePartSet('resFP', ite, 1, this%numFP)
               
                print *, 'Iteration nb ', ite
                print *, '   Time (s) = ', this%currentTime
                print *, '   Time step (s) = ', this%timeStep
                to_save = .false.
            end if
            
            call this%timeStepUpdate()
            call this%slUpdate()
            
            ite = ite + 1
        end do

    end subroutine solver
    
    
    !> particle_manager/timeStepUpdate: computes the next timestep using
    !!               the properties of the particles.
    
    subroutine timeStepUpdate(this)
        class(particle_manager) :: this
        real(DP) :: dTf, dTftemp     !< time step relative to the body forces
        real(DP) :: dTcv, dTcvtemp   !< time step relative to the viscous forces and Courrant number
        integer  :: i
        class(fixed_particle), pointer :: cur_ptr   
        
        ! computes the timestep relative to the body forces     
        dTf = sqrt( this%part(this%numFP+1)%ptr%h / 9.81d0 )
        do i = this%numFP+2, this%numPart
            cur_ptr => this%part(i)%ptr
            dTftemp = sqrt(cur_ptr%h / 9.81d0)
            if(dTftemp<dTf) then
                dTf = dTftemp
            end if
        end do
        
        ! computes the timestep relative to the CN and the viscous forces
        dTcv = this%part(this%numFP+1)%ptr%h &
               / (this%part(this%numFP+1)%ptr%c(1) &
                  + 0.6d0 * (this%alpha * this%part(this%numFP+1)%ptr%c(1) &
                           + this%beta  * this%part(this%numFP+1)%ptr%max_mu_ab ) )
        do i = this%numFP+2, this%numPart
            cur_ptr => this%part(i)%ptr
            dTcvtemp = cur_ptr%h / &
                       (cur_ptr%c(1) &
                       + 0.6d0 * (this%alpha * cur_ptr%c(1) + this%beta * cur_ptr%max_mu_ab))
            if(dTcvtemp<dTcv) then
                dTcv = dTcvtemp
            end if
        end do
    
        ! computes the final timestep
        if(0.4d0*dTf > 0.25d0*dTcv) then
            this%timeStep = 0.25d0 * dTcv
        else
            this%timeStep = 0.4d0 * dTf
        end if
        
        !> possibility to change the timestep if we use the ideal gas law
        if(this%eqnState == 1) then
            this%timeStep = 5.d0 * this%timeStep
        end if
    end subroutine timeStepUpdate


    !> particle_manager/slUpdate is a routine that updates the smoothing length at each timestep.
    !!          It is written to provide the same smoothing length for every particle.
    
    subroutine slUpdate(this)
        class(particle_manager) :: this
        real(DP) :: mean_rho     !< mean value of the densities of the mobile particles
        real(DP) :: new_h        !< new smoothing length
        integer :: i
        mean_rho = 0.d0
        
        ! calculation of the average density
        do i = 1, this%numPart
            mean_rho = mean_rho+this%part(i)%ptr%rho(1)
        end do
        mean_rho = mean_rho/this%numPart
        
        ! calculation of the new smoothing length
        new_h = this%h_0 * (this%rho_0/mean_rho)**(1.d0/3.d0)
        
        ! if the smoothing length is greater than 0.5 the size of a cell, it is limited
        if(new_h > 0.5d0 * this%sorting%cellSize) then
            new_h = 0.5d0 * this%sorting%cellSize
            print *, 'Warning: the smoothing has been limited'
        end if
        
        ! update of the smoothing length
        do i = 1, this%numPart
            this%part(i)%ptr%h = new_h
        end do
    end subroutine slUpdate

    ! ------------------------------------------------------------------------
    ! particle_sort
    ! ------------------------------------------------------------------------

    !> particle_sort/get_h_max: finds the largest smoothing length of the particles.
    !!           This is useful when it is not constant over the particles.
    
    subroutine get_h_max(this)
        class(particle_sort) :: this
        integer :: i
        
        this%h_max = 0
        do i = 1, this%manager%numPart
            if(this%manager%part(i)%ptr%h > this%h_max) then
                this%h_max = this%manager%part(i)%ptr%h
            end if
        end do
        
        ! increase of h_max in order to have a security if h changes.
        ! This is done according to the equation of state used.
        if(this%manager%eqnState == LAW_IDEAL_GAS) then
            this%h_max = 1.1d0 * this%h_max
        else
            this%h_max = 1.02d0 * this%h_max
        end if
    end subroutine get_h_max


    !> particle_sort/setCells : sets the size of the cells in which the particles
    !!    will be sorted. A cell must be cubic. The domain is assumed to be cubic.
    !!    This routine also sets the number of cells and allocates the vector which contains
    !!    the lists of particles.
    !!    In ordrer to be as efficient as possible, the storage vector is not deallocated and
    !!    reallocated at each iteration.
    
    subroutine setCells(this)
        class(particle_sort) :: this
        integer :: ios
        
        ! calculates the necessary number of cells on a side
        call this%get_h_max()
        
        this%nCellsSide = 0
        do while(this%manager%dom_dim / (this%nCellsSide+1) > this%manager%kappa * this%h_max)
            this%nCellsSide = this%nCellsSide + 1
        end do
        
        this%nCells = this%nCellsSide**3

        ! allocated with the necessary number of cells.
        allocate(this%storage(1:this%nCells))
        this%cellSize = this%manager%dom_dim / this%nCellsSide
        
        ! [RB] info
        print *, 'INFO particle_sort/setCells()'
        print *, '   .nCellsSide =', this%nCellsSide
        print *, '   .nCells     =', this%nCells
        print *, '   .cellSize   =', this%cellSize
        
        ! save info 2 disk
        open (unit=1, file="grid.out", action="write", iostat=ios)
        if ( ios /= 0 ) then
            print "('Cannot open grid file')"
            stop
        end if
        write(unit=1, fmt="(I8,E15.7)")   &
                            this%nCellsSide,  &
                            this%cellSize
        close(unit=1)
        
    end subroutine setCells

    
    !> particle_sort/particlesSort: sorts every particle in a cell. 
    !!     This will be useful in order to find the neighbours.
    
    subroutine particlesSort(this)
        class(particle_sort), target :: this
        
        integer :: i
        integer :: xCell, yCell, zCell                       !< number of the cell in the x, y and z direction
        integer :: part_pos                                  !< absolute position of a particle
        real(DP), dimension(:), pointer, contiguous :: xyz   !< position of a particle
        integer, pointer :: nCellsSide                       !< number of cells on a row
        
        class(fixed_particle), pointer :: prt
        
        !print *,"sorting particles"
        
        if(this%init) then
            call this%setCells()
            this%init = .false.
        end if
        
        ! the lists of every cell are reset
        do i = 1, this%nCells
            call this%storage(i)%resetList()
        end do
        
        nCellsSide => this%nCellsSide
        do i = 1, this%manager%numPart
            prt => this%manager%part(i)%ptr
            xyz => prt%coord(:, this%manager%RKstep)
            
            xCell = nint( (xyz(1)-mod(xyz(1), this%cellSize)) / this%cellSize ) + 1
            yCell = nint( (xyz(2)-mod(xyz(2), this%cellSize)) / this%cellSize ) + 1
            zCell = nint( (xyz(3)-mod(xyz(3), this%cellSize)) / this%cellSize ) + 1
            
            if(xCell<1) then
                xCell = 1
            end if
            if(xCell>nCellsSide) then
                xCell = nCellsSide
            end if 
            if(yCell<1) then
                yCell = 1
            end if
            if(yCell>nCellsSide) then
                yCell = nCellsSide
            end if 
            if(zCell<1) then
                zCell = 1
            end if
            if(zCell>nCellsSide) then
                zCell = nCellsSide
            end if 
            
            part_pos = (xCell-1)*nCellsSide**2 + (yCell-1)*nCellsSide + zCell
        
            call this%storage(part_pos)%addElement( prt, 0.0_dp )
        end do
    
    end subroutine particlesSort

end module SPH_module
