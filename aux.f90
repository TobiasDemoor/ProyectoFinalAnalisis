program aux
    implicit none
    real(8), parameter :: UA = 149.6e9, G = 6.67384e-11, M = 1.9891e30
    real(8) :: V(2)
    integer :: i

    open(2, file = "eccA.txt")
    open(3, file = "vel.dat")
    do i = 1, 8
        read(2, *)V
        write(3, *) sqrt(((1+V(2))*M*G)/((1-V(2))*V(1)*UA))
    end do
    close(3)
    close(2)
end program aux