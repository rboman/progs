! Workaround to use an array of pointers in class 'shapeSet'

module domainptr_m

    use shape_m

    type domainptr
        class(shape), pointer :: p
    end type domainptr

end module domainptr_m
