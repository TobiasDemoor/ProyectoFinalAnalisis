module edo
contains
    function Vprima(V, M)
        implicit none
        real(8), allocatable :: Vprima(:,:,:)
        real(8) :: V(0:,:,:), M(:)
        real(8), parameter :: G = 6.67384e-11
        real(8) :: Aux(3)
        integer :: n, i, j

        n = size(V, dim = 3)
        allocate(Vprima(0:3, 3, n))
        Vprima(0,:,:) = 1.0
        Vprima(1,:,:) = V(2,:,:) ! x' = v
        Vprima(2,:,:) = V(3,:,:) ! v' = a
        do i = 1, n
            Vprima(3,:,i) = 0
            do j = 1, i-1
                Aux = G * M(i)*M(j)/sqrt((V(1,:,i)-V(1,:,j))**2) * (V(1,:,j)-V(1,:,i))/abs(V(1,:,j)-V(1,:,i))
                Vprima(3,:,i) = Vprima(3,:,i) + Aux
            end do
            do j = i+1, n
                Aux = G * M(i)*M(j)/sqrt((V(1,:,i)-V(1,:,j))**2) * (V(1,:,j)-V(1,:,i))/abs(V(1,:,j)-V(1,:,i))
                Vprima(3,:,i) = Vprima(3,:,i) + Aux
            end do
            Vprima(3,:,i) = Vprima(3,:,i)/M(i)
        end do
    end function Vprima
    
    function rk4SP(V, M, h)
        real(8) :: V(:,:,:), M(:)
        real(8), allocatable, dimension(:,:,:)  :: rk4SP, K1, K2, K3, K4
        real(8) :: h
        integer :: n

        n = size(V, dim = 3)
        allocate(rk4SP(0:3, 3, n), K1(0:3, 3, n), K2(0:3, 3, n), K3(0:3, 3, n), K4(0:3, 3, n))
        K1 = h*Vprima(V, M)
        K2 = h*Vprima(V+K1/2.0, M)
        K3 = h*Vprima(V+K2/2.0, M)
        K4 = h*Vprima(V+K3, M)
        rk4SP = V + (K1 + 2.0*K2 + 2.0*K3 +K4)/6.0
        deallocate(K1, K2, K3, K4)
    end function

    subroutine est1(V, M, h, tol)
        implicit none
        real(8), intent(in) :: V(:,:,:), M(:), tol
        real(8), intent(inout) :: h
        real(8) :: error
        real(8), allocatable :: E(:,:,:)
        integer :: n

        n = size(V, dim = 3)
        allocate(E(0:3,3,n))
        do while(.true.)
            E = rk4SP(V, M, h) - rk4SP(rk4SP(V, M, h/2), M, h/2)
            error = maxval(abs(E))
            if(error>=tol) then
                h = h/2.0
            else
                if(5*error<tol) then
                    h = h*2
                else
                    go to 90
                end if
            end if
        end do
        90 print *
    end subroutine est1

    subroutine rk4(Vi, M, h, tfinal, hModif, tol)
        implicit none
        logical,intent(in) :: hModif
        real(8), intent(in) :: tfinal, tol, Vi(0:,:,:), M(:)
        real(8), intent(inout) :: h
        real(8), allocatable :: V(:,:,:)
        integer :: n

        n = size(Vi, dim = 3)
        allocate(V(0:3,3,n))
        open(2, file = "datos.txt")
        write(2, *) Vi
        V = Vi
        do while (V(0,1,1) <= tfinal)
            if (hModif) then
                write (3, '(F15.7)') h
                call est1(V, M, h, tol)
            end if
            V = rk4SP(V, M, h)
            write (2, *) V
        end do
        close(3)
        deallocate(V)
    end subroutine rk4
end module edo