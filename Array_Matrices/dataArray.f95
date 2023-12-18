! input values using data statement
program dataArray 
    implicit none
    integer, dimension(5) :: a 
    integer, dimension(2,3) :: mat
    integer :: i, j

    data a /1, 2, 3, 4, 5/

    data mat(1,:) /1, 1, 1/
    data mat(2,:) /2, 2, 2/

    print*,"-----a-----"
    print*,a

    print*,"-----mat-----"
    do i = 1,2
        print*,(mat(i,j), j = 1,3)
    end do 

    ! "where" statement use
    where (mat < 2)
        mat = 0
    elsewhere
        mat = 100 
    end where 

    print*,"-----mat modified with where statement-----"
    do i = 1,2
        print*,(mat(i,j), j = 1,3)
    end do

end program dataArray 