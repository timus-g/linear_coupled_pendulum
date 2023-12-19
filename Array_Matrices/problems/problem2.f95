! Q. print out
! 1, 2, 3, 4, 5
! 1, 4, 9, 16, 25
! 1, 8, 27, 64, 125

program problem2
    implicit none
    integer, dimension(5) :: a, a2, a3 
    integer :: i 

    do i = 1, size(a)
        a(i) = i 
    end do 

    a2 = a**2 !squaring each element
    a3 = a**3 !cubing each element

    print*,a 
    print*,"Square:"
    print*,a2
    print*,"Cube:"
    print*,a3

end program problem2