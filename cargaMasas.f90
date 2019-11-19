program cargaMasas
    implicit none
    real(8), parameter :: G = 6.67384e-11, rad = 0.0174532925
    real(8), allocatable :: V(:,:,:), M(:)
    real(8) :: Aux(3), Vel, U
    integer :: i, n

    open(2, file = "Ejemplos/sistSolar.txt")
    read(2, *) n
    read(2, *) U
    allocate(V(2,3,n+1), M(n+1))
    read(2, *) M(1)
    V(:,:,:) = 0
    do i = 2, n+1
        read(2, *) M(i)
        read(2, *) Aux
        V(1,:,i) = 0
        V(1,1,i)= (1-Aux(2))*Aux(1)*U
        V(2,:,i) = 0
        Vel = sqrt(((1+Aux(2))*M(1)*G)/((1-Aux(2))*Aux(1)*U))
        V(2,2,i) = Vel*cos(Aux(3)*rad)
        V(2,3,i) = Vel*sin(Aux(3)*rad)
    end do
    close(2)
    ! Ajusto sum(cant movimiento)
    do i = 1, 3
        V(2,i,1) = - sum(V(2,i,2:)*M(2:))/M(1)
    end do
    open(3, file = "sistSolar.dat")
    write(3, *) n+1
    do i = 1, n+1
        write(3, *) M(i)
        write(3, *) V(1,:,i)
        write(3, *) V(2,:,i)
    end do
    close(3)
end program cargaMasas