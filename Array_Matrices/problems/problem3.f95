! Q print the Fibonacci sequence | f(i) = f(i-1) + f(i-2)
! 0 1 1 2 3 5 8 13 21

program Fibonacci
    implicit none
    integer, dimension(:), allocatable :: f
    integer :: n, i

    print*,"Enter the size of the fibonacci sequency:"
    read*,n 
    allocate(f(n))
    f(1) = 0; f(2) = 1 ! give the values for the first two position

    do i = 3, n
        f(i) = f(i-1) + f(i-2)
    end do 

    print*,"The fibonacci sequence:"
    print*,f 

    deallocate(f)
end program Fibonacci