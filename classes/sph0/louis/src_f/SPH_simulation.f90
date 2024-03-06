!> SPH simulation
!! @n This program is used to solve the Navier-Stokes equations
!!    using the SPH method. A number of files must be given:
!!    paths.txt, *.prm, *.fp and *.mp.
!! @warning The domain must be cubic!
!! @brief   Main program to launch a SPH simulation
!! @author  Louis Goffin
!! @date    2013-05-26
!! @version 1.0.0

!! print info about Debug/Release mode

subroutine info_debug()
    use iso_fortran_env
    logical :: compiled_with_g
    character(:), allocatable :: options
  
    options = compiler_options()
  
    compiled_with_g =  index(options, "-g") > 0
    if(compiled_with_g) then
        print *, "code built in DEBUG mode."
    else
        print *, "code built in RELEASE mode."
    end if
    deallocate(options)
end subroutine info_debug

!! print info about OpenMP availability

subroutine info_openmp()
    
    use omp_lib
    logical :: compiled_with_openmp = .false.

    !$ compiled_with_openmp = .true.
    if (compiled_with_openmp) then
        !$ print *,'OpenMP available: OMP_NUM_THREADS=', OMP_GET_MAX_THREADS()
    else
        print *,'OpenMP not available.'
    end if
end subroutine info_openmp


program SPH_simulation

    use SPH_module
    implicit none
    
    integer :: t1, t2, clock_rate, clock_max
    type(particle_manager) :: manager

    call info_openmp()
    call info_debug()

    print *,'============= SPH_simulation (L.Goffin)'
    
    call system_clock(t1, clock_rate, clock_max)
    
    call manager%initialisation()
    call manager%solver()

    call system_clock(t2, clock_rate, clock_max)
    
    print *,'Elapsed real time = ', DBLE(t2-t1)/clock_rate

end program SPH_simulation

