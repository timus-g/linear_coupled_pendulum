!array-usage

program array
    implicit none
    !integer, dimension(6) :: a ! uncomment to use
    !another way of assigning arrays
    integer :: a(6)
    integer :: i 

    !storing values in a 
    !a(1) = 10 ! uncomment to use
    !a(2) = 11
    !a(3:5)= 63 !giving values from a(3) to a(5)
    !a(6) = 21

    ! another way
    a = (/10,11,63,63,63,21/)
    print*,a+5

    do i=1,6
        print*,a(i)
    end do
end program
