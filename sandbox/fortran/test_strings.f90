
program test_strings

    implicit none
    character*30 name_file

    name_file = 'contact1d_r   '
    print *, '"', name_file, '"'

    if (name_file == 'contact1d_r') then ! ne tient pas compte des espaces!
        print *, 'true'
    else
        print *, 'false'
    end if

    print *, '"', name_file//'.txt', '"'
    print *, '"', TRIM(name_file)//'.txt', '"'

end
