! intrinsic array functions
program arrayFunctions
    implicit none
    integer :: i, j 
    !vectors
    real, dimension(2) :: a, b
    !matrices
    real, dimension(2,2) :: matA, matB, matC 
    
    ! vector calculations
    data a /2.12, -4.23/
    data b /32.5, 4.65/
    ! vector dot product
    print*,dot_product(a,b)

    !matrix calculations
    data matA(1,:) /1.0,0.0/ !Identity matrix
    data matA(2,:) /0.0,1.0/ 
    
    matB = reshape((/2.2, 3.2, 4.9, 8.5/),(/2,2/))

    !product of two matrices
    matC = matmul(matA,matB)

    do i = 1,2
        print*,(matC(i,j), j = 1,2)
    end do

    !some other functions are |size() to get the number of elements
    !shape() to get the row x columns
    !rank() to get the dimensions
    !sum() sum of all the elements
    !product() product of all the elements
    !count(...condition...) thus gives a count of elements based on the condition like >,<,= etc.
    !this (...condition...) works for all the above functions
    !maxval() maximum value
    !minval() minimum value
    !lbound() & ubound() lower and upper bound of the array based on the index
end program arrayFunctions