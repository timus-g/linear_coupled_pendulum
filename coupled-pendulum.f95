! Runge Kutta 4th order to solve set of first order differential equations
! Three coupled pendulum dynamics

PROGRAM pendulum
    IMPLICIT none

    ! Declarations
    ! t : time (initial value given)
    ! x(1,2,3) : position of the bobs
    ! v(1,2,3) : velocity (=dx/dt)
    ! dt : step-size (=0.01)
    ! f(1,2,3) : dx/dt 
    ! g(1,2,3) : dv/dt
    ! k : spring constant
    ! l : length of the rod
    ! g : acceleration due to gravity
    integer :: n
    real*8 :: t,twant,dt,f,g
    real*8, allocatable, dimension(:) :: x,v
    real*8, allocatable, dimension(:,:) :: x_data,xt,vt
    integer:: niter, i, j, k, z

    write(*,*)"Enter the number of bobs"
    read(*,*) n
    allocate(x(1:n)); allocate(v(1:n))
    allocate(xt(1:n,4)); allocate(vt(1:n,4))

    t=0.0d0; x(1)=0.02; x(2)=0.00; x(3)=-0.02; dt=0.01d0 ; twant=100.0d0 ; niter=int((twant-t)/dt)

    allocate(x_data(1:niter,n))

    open(unit=50, file="three-pendulum_normal2.dat", action="write")
    open(unit=51, file="three-pendulum_normal2.xyz", action="write")

    do i=1,niter
        ! Step 1
        xt(1,1) = dt*f(v(1))
        vt(1,1) = dt*g(x(1),x(1),x(2))
        do j = 2,n-1
            xt(j,1)=dt*f(v(j))
            vt(j,1)=dt*g(x(j-1),x(j),x(j+1))
        end do
        xt(n,1) = dt*f(v(n))
        vt(n,1) = dt*g(x(n-1),x(n),x(n))

        ! Step 2 & 3
        do k = 2,3
            xt(1,k) = dt*f(v(1))
            vt(1,k) = dt*g(x(1)+0.5*xt(1,k-1),x(1)+0.5*xt(1,k-1),x(2)+0.5*xt(2,k-1))
            do j = 2,n-1
                xt(j,k)=dt*f(v(j))
                vt(j,k)=dt*g(x(j-1)+0.5*xt(j-1,k-1),x(j)+0.5*xt(j,k-1),x(j+1)+0.5*xt(j+1,k-1))
            end do
            xt(n,k) = dt*f(v(n))
            vt(n,k) = dt*g(x(n-1)+0.5*xt(n-1,k-1),x(n)+0.5*xt(n,k-1),x(n)+0.5*xt(n,k-1))
        end do

        !Step 4
        xt(1,4) = dt*f(v(1))
        vt(1,4) = dt*g(x(1)+xt(1,3),x(1)+xt(1,3),x(2)+xt(2,3))
        do j = 2,n-1
            xt(j,4)=dt*f(v(j))
            vt(j,4)=dt*g(x(j-1)+xt(j-1,3),x(j)+xt(j,3),x(j+1)+xt(j+1,3))
        end do
        xt(n,4) = dt*f(v(n))
        vt(n,4) = dt*g(x(n-1)+xt(n-1,3),x(n)+xt(n,3),x(n)+xt(n,3))
        
        do z = 1,n
            x(k) = x(k)+(xt(k,1)+2.0*xt(k,2)+2.0*xt(k,3)+xt(k,4))*(1.0/6.0)
            v(k) = v(k)+(vt(k,1)+2.0*vt(k,2)+2.0*vt(k,3)+vt(k,4))*(1.0/6.0)
        end do

    end do
end program pendulum

!We define our function here : dx/dt for f
    real*8 function func_f(v)
        real*8 :: v
        func_f=v
    end function

! We define our function here : dv/dt for g
    real*8 function g(x1,x2,x3)
        real*8 :: x1,x2,x3,l,m,k,gacc
        l=0.34; m=0.0754; k=0.6; gacc=9.81
        g = -(gacc/l)*x2 + (k/m)*(x1-x2) + (k/m)*(x3-x2)
    end function

