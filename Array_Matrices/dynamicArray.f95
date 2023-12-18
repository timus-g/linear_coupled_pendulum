! Using dynamic array

program dynamicArray 
    implicit none
    integer, dimension(:,:), allocatable :: matA ! an allocatable 2D array
    integer :: size1, size2
    integer :: i, j

    write(*,*)"Enter the dimention sizes of the 2D array ..."
    write(*,*)"Enter the number of rows-"
    read*,size1 !we can also write read(*,*)size1 while reading the user input
    write(*,*)"Enter the number of columns-"
    read*,size2

    !allocate the array
    allocate(matA(size1,size2))

    !assignig values to the matrix
    do i = 1, size1
        do j = 1, size2
            matA(i,j) = i+j
        end do 
    end do 

    do i = 1, size1
        print*,(matA(i,j), j = 1, size2)
    end do 

    !always deallocate after finishing the work
    deallocate(matA)
end program dynamicArray