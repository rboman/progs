!> SPH simulation
!! @n This program is used to solve the Navier-Stokes equations
!!    using the SPH method. A number of files must be given:
!!    paths.txt, *.prm, *.fp and *.mp.
!! @warning The domain must be cubic!
!! @brief   Main program to launch a SPH simulation
!! @author  Louis Goffin
!! @date    2013-05-26
!! @version 1.0.0

program SPH_simulation

    use SPH_module
    implicit none
    
    integer :: t1, t2, clock_rate, clock_max
    type(particle_manager) :: manager

    print *,'============= SPH_simulation (L.Goffin)'
    
    call system_clock(t1, clock_rate, clock_max)
    
    call manager%initialisation()
    call manager%solver()

    call system_clock(t2, clock_rate, clock_max)
    
    print *,'Elapsed real time = ', (t2-t1)/clock_rate

end program SPH_simulation

