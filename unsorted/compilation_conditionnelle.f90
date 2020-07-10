program foo

implicit none

CHARACTER(len=10) version




! This file contained simple examples of conditional compilation. The compilation preprocessor is usually a C processor. 
! Therefore, the synthax used to formulated the conditional statements is in C language and must be flagged by a # (commentar).
! Because it involve C language mixed with Fortran language, one single compiler is not enought. For this, the preprocessor is 
! is a C compilor, running all the flagged task (with #) and writing a modified Fortran Script, which is compiled as usual with 
! a fortran compiler. To lauch the compiling tasks, you must use the flag -cpp (for "C Pre-Processor")
! example : gfortran -cpp main.f90 or pgfortran -cpp main.f90.
! Some predefined variables are build in in the preprocessor (example, the compiler name, ...). You may find further information
! about these preprocessor macros with the command "gfortran -cpp -E -dM maing.f90". Some are common between all compiler, but 
! some are not (for example, __linux macro exist with ifort and pgfortran, but not on gfortran.)




#if defined _WIN32 

    ! WIN32 is turned on for 32 and 64 bits architectures
    print *, 'Hello from Windows OS!'


#elif defined __linux

    print *, "Hello from Linux OS"

#else

    print *, 'Hello from an unknown exploitation system'

#endif







#if defined __GFORTRAN__

    write ( version, '(i1,".",i1,".",i1)' ) __GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__
    print *, 'This code was compiled with GNU gfortran Compiler ' // trim( version ) // '.'
    ! ask "gfortran -cpp -dM -E" in terminal for more preprocessor predefined macros


#elif defined __INTEL_COMPILER

    write ( version, '(i6)' ) __INTEL_COMPILER_BUILD_DATE
    print *, 'This code was compiled using Intel Fortran Comppiler (Release Date: ' // trim( version ) //  ').'


#elif defined __PGI

    write ( version, '(i2,".",i2,".",i1)' ) __PGIC__, __PGIC_MINOR__, __PGIC_PATCHLEVEL__
    print *, 'This code was compiled using PGI Compiler ' // trim( version ) // '.'
    ! ask "pgcc -dM -E" in terminal for more preprocessor predefined macros


#else 

    print *, 'This code was compiled using an unknown compiler...'

#endif






end program foo