! Q. print ouname
! 1 0 1 0 1 0 .... 20 times

program problem1
    implicit none
    integer :: a(20) ! creating a array with length 20
    integer :: i     ! for looping 

    do i = 1,20
        a(i) = 1                    ! give every input 1
        if (mod(i,2) == 0) a(i) = 0 ! change the input to 0 if i is even
    end do 

    print*,a
end program problem1