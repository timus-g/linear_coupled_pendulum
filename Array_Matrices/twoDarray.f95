! 2D arrays
program twoDarray
    implicit none
    integer :: matA(2,3)
    integer :: i, j

    !assigning values
    !using indices one by one
    !matA(1,1) = 23; matA(1,2) = 42; matA(1,3) = 100 
    !matA(2,1) = 1; matA(2,2) = 3; matA(2,3) = 5

    !shorthand way
    matA = reshape((/23, 42, 100, 1, 3, 5/),(/2,3/)) !transpose(reshape(...)) works for transposing a squate matrix only!!!

    !printing the values ! uncomment as required
    !print*,matA ! if we print like this everything will be printed in a single row 

    !using nested loop ! but this prints in a single column
    !do i = 1,2
    !    do j = 1,3
    !       print*,matA(i,j)
    !    end do
    !end do

    !another nested loop to give it a matrix look
    do i = 1,2
        print*,(matA(i,j), j = 1,3)
    end do 

end program twoDarray