module edo
contains 

    function Vprima(V, M)
        implicit none
        real(8), allocatable :: Vprima(:,:,:)
        real(8) :: V(:,:,:), M(:)
        real(8), parameter :: G = 6.67384e-11
        real(8) :: F, distcuad
        integer :: n, i, j

        n = size(V, dim = 3)
        allocate(Vprima(2, 3, n))
        Vprima(1,:,:) = V(2,:,:) ! x' = v
        do i = 1, n ! v' = a
            Vprima(2,:,i) = 0
            do j = 1, i-1
                distcuad = sum((V(1,:,j)-V(1,:,i))**2)  ! distancia al cuadrado
                F = G * M(i)*M(j)/distcuad              ! Modulo de F
                Vprima(2,:,i) = Vprima(2,:,i) + F * (V(1,:,j)-V(1,:,i))/sqrt(distcuad)
            end do
            do j = i+1, n
                distcuad = sum((V(1,:,j)-V(1,:,i))**2)  ! distancia al cuadrado
                F = G * M(i)*M(j)/distcuad              ! Modulo de F
                Vprima(2,:,i) = Vprima(2,:,i) + F * (V(1,:,j)-V(1,:,i))/sqrt(distcuad)
            end do
            Vprima(2,:,i) = Vprima(2,:,i)/M(i)
        end do
    end function Vprima
    
    function rk4SP(V, M, h)
        real(8) :: V(:,:,:), M(:)
        real(8), allocatable, dimension(:,:,:)  :: rk4SP, K1, K2, K3, K4
        real(8) :: h
        integer :: n

        n = size(V, dim = 3)
        allocate(rk4SP(2, 3, n), K1(2, 3, n), K2(2, 3, n), K3(2, 3, n), K4(2, 3, n))
        K1 = h*Vprima(V, M)
        K2 = h*Vprima(V+K1/2.0, M)
        K3 = h*Vprima(V+K2/2.0, M)
        K4 = h*Vprima(V+K3, M)
        rk4SP = V + (K1 + 2.0*K2 + 2.0*K3 +K4)/6.0
        deallocate(K1, K2, K3, K4)
    end function

    subroutine ajusteH(V, M, h, tol)
        implicit none
        real(8), intent(in) :: V(:,:,:), M(:), tol
        real(8), intent(inout) :: h
        real(8) :: error
        real(8), allocatable :: E(:,:,:)
        integer :: n

        n = size(V, dim = 3)
        allocate(E(2, 3, n))
        do while(.true.)
            E = rk4SP(V, M, h) - rk4SP(rk4SP(V, M, h/2), M, h/2)
            error = maxval(abs(E))
            if(error>=tol) then
                h = h/2.0
            else
                if(1e6*error<tol) then ! con esto a veces lo aumenta a si que es razonable
                    h = h*2
                else
                    ! aca corta las modificaciones del h
                    go to 90
                end if
            end if
        end do
        90 print *
    end subroutine ajusteH

    function str(k)
        ! "Convert an integer to string."
        ! Q: ¿Dónde conseguiste este codigo Timmy?
        ! A: Internet
        character(len=20) :: str
        integer, intent(in) :: k
        write (str, *) k
        str = adjustl(str)
    end function str

    subroutine scriptGnuplot(nMasas)
        implicit none
        integer, intent(in) :: nMasas
        integer :: i

        open(9, file = "sgnptMasas.p", status = "REPLACE") !podriamos mandar el nombre como parametro pero meh

        write(9,"(A)")"set nokey"
        write(9,"(A)")"set xlabel 'x'"
        write(9,"(A)")"set ylabel 'y'"
        write(9,"(A)")"set zlabel 'z'"
        write(9,"(A)")"splot \"

        do i = 1, nMasas-1
            write(9, "(A)")"'fort."//trim(str(i))//"' with lines ls "//trim(str(3*i))//" lw 3,\"
        end do 
        write(9, "(A)")"'fort."//trim(str(i))//"' with lines ls "//trim(str(3*i))//" lw 3"

        close(9)
    end subroutine scriptGnuplot

    subroutine rk4(Vi, M, h, tfinal, hModif, tol)
        implicit none
        logical, intent(in) :: hModif
        real(8), intent(in) :: tfinal, tol, Vi(:,:,:), M(:)
        real(8), intent(inout) :: h
        real(8), allocatable :: V(:,:,:)
        real(8) :: t = 0 ! el tiempo lo llevamos como variable local solo para verificar t < tfinal
        integer :: n, i

        n = size(Vi, dim = 3)
        allocate(V(2, 3, n))

        do i = 1, n
            open(i)
        end do
        open(99, file = "h.txt")
        
        do i = 1, n
            write (i, *) Vi(1,:,i)
        end do
        V = Vi
        do while (t < tfinal)
            if (hModif) then
                call ajusteH(V, M, h, tol)
                write(99, *) h
            end if
            V = rk4SP(V, M, h)
            t = t + h
            do i = 1, n
                write (i, *) V(1,:,i)
            end do
        end do

        close(99)
        do i = 1, n
            close(i)
        end do
        
        deallocate(V)
    end subroutine rk4

end module edo