! see http://fortranwiki.org/fortran/show/c_interface_module

! l'idée est de passer une chaine C et de récupérer une chaine fortran

subroutine cstr_to_fstr(chainec, chainef)
    use, intrinsic :: iso_c_binding
    implicit none
    character(len=1, kind=C_char), intent(in) :: chainec(*)
    character(len=*) :: chainef
    integer :: i
    i=1
    do while(chainec(i)/=C_NULL_char .and. i<=len(chainef))
        chainef(i:i) = chainec(i)
        i=i+1
    end do
    if (i<len(chainef)) chainef(i:) = ' '
end

subroutine froutine(entier, chainec, chainelen) BIND(C, name="froutine")

    use, intrinsic :: iso_c_binding
    implicit none
    integer(kind=C_INT), intent(in) :: entier

    integer(kind=C_INT), intent(in) :: chainelen
    character(len=1,kind=C_char), intent(in) :: chainec(*)
    character(len=chainelen) :: chainef

    print *, 'in froutine()'
    print *, '  entier =', entier
    ! copy c string to f string
    call cstr_to_fstr(chainec, chainef)
    print *, '  chaine = "', chainef,'"'
end

