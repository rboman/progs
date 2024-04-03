!> functions added by RB

module mod_time

    use OMP_LIB

    implicit none

    !> timer structure
    type s_timer
        private
        real*8 :: start_time = 0.0d0
        real*8 :: elaps_time = 0.0d0
        logical :: started = .false.
    contains
        procedure :: start => s_timer_start
        procedure :: stop => s_timer_stop
        procedure :: reset => s_timer_reset
        procedure :: read => s_timer_read
        procedure :: print => s_timer_print
    end type s_timer

    type(s_timer) :: timer_total      
    type(s_timer) :: timer_copy_vars     
    type(s_timer) :: timer_initialisation
    type(s_timer) :: timer_save
    type(s_timer) :: timer_sort
    type(s_timer) :: timer_update_dt
    type(s_timer) :: timer_update_h
    type(s_timer) :: timer_update_vars

contains

    !> Start the timer from last "elapsed time"
    subroutine s_timer_start(this)
        class(s_timer) :: this
        this%started = .true.
!$      if (.true.) then
!$          this%start_time = omp_get_wtime()
!$      else
            call cpu_time(this%start_time)
!$      end if
    end subroutine s_timer_start

    !> Stop the timer
    subroutine s_timer_stop(this)
        class(s_timer) :: this
        this%elaps_time = this%read()
        this%start_time = 0.0d0
        this%started = .false.
    end subroutine s_timer_stop

    !> Reset the timer
    subroutine s_timer_reset(this)
        class(s_timer) :: this
        this%start_time = 0.0d0
        this%elaps_time = 0.0d0
        this%started = .false.
    end subroutine s_timer_reset

    !> Read current time (without stopping the timer)
    function s_timer_read(this) result(now)
        class(s_timer), intent(in) :: this
        real*8 :: now, tm
        if (this%started) then
!$          if (.true.) then
!$              tm = omp_get_wtime()
!$          else
                call cpu_time(tm)
!$          end if
            now = this%elaps_time + tm - this%start_time
        else
            now = this%elaps_time
        end if
    end function s_timer_read

    !> Print the timer
    subroutine s_timer_print(this)
        class(s_timer), intent(in) :: this
        print *, 'timer =', this%read()
    end subroutine s_timer_print

    !> print statistics
    subroutine time_print_stats

        ! read/compute times
        real*8 t_total, t_copy_vars, t_initialisation, t_save, t_sort, t_update_dt, t_update_h, t_update_vars
        ! percentages
        real*8 p_total, p_copy_vars, p_initialisation, p_save, p_sort, p_update_dt, p_update_h, p_update_vars

        t_total = timer_total%read()
        t_copy_vars = timer_copy_vars%read()
        t_initialisation = timer_initialisation%read()
        t_save = timer_save%read()
        t_sort = timer_sort%read()
        t_update_dt = timer_update_dt%read()
        t_update_h = timer_update_h%read()
        t_update_vars = timer_update_vars%read()

        ! compute percentages
        p_copy_vars = t_copy_vars/t_total*100.0d0
        p_initialisation = t_initialisation/t_total*100.0d0
        p_save = t_save/t_total*100.0d0
        p_sort = t_sort/t_total*100.0d0
        p_update_dt = t_update_dt/t_total*100.0d0
        p_update_h = t_update_h/t_total*100.0d0
        p_update_vars = t_update_vars/t_total*100.0d0
        p_total = 100.0d0

        ! display
        print "(/,A)", 'Timers:'
        print "(A)", '-------'
        print "(A, F12.2, A, F10.2, A)", ' . TOTAL           :', t_total, ' sec', p_total, ' %'
        print "(A, F12.2, A, F10.2, A)", ' . copy_vars       :', t_copy_vars, ' sec', p_copy_vars, ' %'
        print "(A, F12.2, A, F10.2, A)", ' . initialisation  :', t_initialisation, ' sec', p_initialisation, ' %'
        print "(A, F12.2, A, F10.2, A)", ' . save            :', t_save, ' sec', p_save, ' %'
        print "(A, F12.2, A, F10.2, A)", ' . sort            :', t_sort, ' sec', p_sort, ' %'
        print "(A, F12.2, A, F10.2, A)", ' . update_dt       :', t_update_dt, ' sec', p_update_dt, ' %'
        print "(A, F12.2, A, F10.2, A)", ' . update_h        :', t_update_h, ' sec', p_update_h, ' %'
        print "(A, F12.2, A, F10.2, A)", ' . update_vars     :', t_update_vars, ' sec', p_update_vars, ' %'
        print "(/)"

    end subroutine time_print_stats

    !> returns the current date and time as a string
    !! (does not require IFPORT or a gcc extension)
    subroutine get_date_and_time(thedate)

        character(len=19) :: thedate

        integer date_time(8)
        character*10 b(3)
        call date_and_time(b(1), b(2), b(3), date_time)

        ! print *, 'date_time    array values:'
        ! print *, 'year=', date_time(1)
        ! print *, 'month_of_year=', date_time(2)
        ! print *, 'day_of_month=', date_time(3)
        ! print *, 'time difference in minutes=', date_time(4)
        ! print *, 'hour of day=', date_time(5)
        ! print *, 'minutes of hour=', date_time(6)
        ! print *, 'seconds of minute=', date_time(7)
        ! print *, 'milliseconds of second=', date_time(8)
        ! print *, 'DATE=', b(1)
        ! print *, 'TIME=', b(2)
        ! print *, 'ZONE=', b(3)

        write (thedate, "(I4,'-',I0.2,'-',I0.2,' ',I0.2,':',I0.2,':',I0.2)") &
            date_time(1), date_time(2), date_time(3), &
            date_time(5), date_time(6), date_time(7)
    end subroutine get_date_and_time

end module mod_time
