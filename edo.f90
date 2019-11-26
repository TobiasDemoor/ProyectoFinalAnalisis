module edo
contains 
    subroutine LeeMasas(Vi, M, file, d)
        implicit none
        real(8), intent(out), allocatable :: Vi(:,:,:), M(:)
        integer, intent(in) :: file, d
        real(8) :: aux(d)
        integer :: n, i

        read(file, *) n
        allocate(Vi(2,d,n),M(n))
        do i = 1, n
            read(file, *) M(i)
            read(file, *) aux
            Vi(1,:,i) = aux
            read(file, *) aux
            Vi(2,:,i) = aux
        end do
        close(file)
    end subroutine LeeMasas

    function Vprima(V, M)
        implicit none
        real(8), allocatable :: Vprima(:,:,:)
        real(8) :: V(:,:,:), M(:)
        real(8), parameter :: G = 6.67384e-11
        real(8) :: F, distcuad
        integer :: d, n, i, j

        d = size(V, dim = 2)
        n = size(V, dim = 3)
        allocate(Vprima(2, d, n))
        Vprima(1,:,:) = V(2,:,:) ! x' = v
        do i = 1, n ! v' = a
            Vprima(2,:,i) = 0
            do j = 1, i-1
                distcuad = sum((V(1,:,j)-V(1,:,i))**2)  ! distancia al cuadrado
                F = G * M(j)/distcuad              ! Modulo de F
                Vprima(2,:,i) = Vprima(2,:,i) + F * (V(1,:,j)-V(1,:,i))/sqrt(distcuad)
            end do
            do j = i+1, n
                distcuad = sum((V(1,:,j)-V(1,:,i))**2)  ! distancia al cuadrado
                F = G * M(j)/distcuad              ! Modulo de F
                Vprima(2,:,i) = Vprima(2,:,i) + F * (V(1,:,j)-V(1,:,i))/sqrt(distcuad)
            end do
        end do
    end function Vprima

    function EulerSP(V, M, h)
        real(8), allocatable :: EulerSP(:,:,:)
        real(8) :: V (:,:,:), M(:)
        real(8) :: h
        integer :: d, n

        d = size(V, dim = 2)
        n = size(V, dim = 3)
        allocate(EulerSP(2, d, n))
        EulerSP = V + Vprima(V, M) * h
    end function

    function EulerMejorSp(V, M, h)
        real(8), allocatable, dimension(:,:,:)  :: EulerMejorSp
        real(8) :: V (:,:,:), M(:)
        real(8) :: h
        integer :: d, n

        d = size(V, dim = 2)
        n = size(V, dim = 3)
        allocate(EulerMejorSp(2, d, n))
        EulerMejorSp = V + h*Vprima(EulerSP(V, M, h/2), M)
    end function

    function rk4SP(V, M, h)
        real(8), allocatable, dimension(:,:,:)  :: rk4SP, K1, K2, K3, K4
        real(8) :: V(:,:,:), M(:)
        real(8) :: h
        integer :: d, n

        d = size(V, dim = 2)
        n = size(V, dim = 3)
        allocate(rk4SP(2, d, n), K1(2, d, n), K2(2, d, n), K3(2, d, n), K4(2, d, n))
        K1 = h*Vprima(V, M)
        K2 = h*Vprima(V+K1/2.0, M)
        K3 = h*Vprima(V+K2/2.0, M)
        K4 = h*Vprima(V+K3, M)
        rk4SP = V + (K1 + 2.0*K2 + 2.0*K3 +K4)/6.0
        deallocate(K1, K2, K3, K4)
    end function

    function rkfSP(V, M, h)
        real(8), allocatable, dimension(:,:,:)  :: rkfSP, K1, K2, K3, K4, K5, K6
        real(8) :: V(:,:,:), M(:)
        real(8) :: h
        integer :: d, n

        d = size(V, dim = 2)
        n = size(V, dim = 3)
        allocate(rkfSP(2, d, n), K1(2, d, n), K2(2, d, n), K3(2, d, n), K4(2, d, n), K5(2, d, n), K6(2, d, n))
        K1 = h*Vprima(V, M)
        K2 = h*Vprima(V + K1/4.0, M)
        K3 = h*Vprima(V + (3.0*K1 + 9.0*K2)/32.0, M)
        K4 = h*Vprima(V + (1932.0*K1 - 7200.0*K2 + 7296.0*K3)/2197.0, M)
        K5 = h*Vprima(V + 439.0*K1/216.0 - 8.0*K2 + 3680.0*K3/513.0 - 845.0*K4/4104.0, M)
        K6 = h*Vprima(V - 8.0*K1/27.0 + 2.0*K2 - 3544.0*K3/2565.0 + 1859.0*K4/4104.0 - 11.0*K5/40.0, M)
        rkfSP = V + (25.0*K1/216.0 + 1408.0*K3/2565.0 + 2197.0*K4/4104.0 - K5/5.0)
        deallocate(K1, K2, K3, K4, K5, K6)
    end function

    function MetodosSP(V, M, h, tipo)
        implicit none
        real(8), allocatable :: MetodosSP(:,:,:)
        real(8) :: V(:,:,:), M(:)
        real(8) :: h
        integer :: tipo, d, n

        d = size(V, dim = 2)
        n = size(V, dim = 3)
        allocate(MetodosSP(2, d, n))
        select case (tipo)
        case (1)
            MetodosSP = EulerSp(V, M, h)
        case (2)
            MetodosSP = EulerMejorSp(V, M, h)
        case (3)
            MetodosSP = rk4SP(V, M, h)
        case (4)
            MetodosSP = rkfSP(V, M, h)
        end select
    end function MetodosSP

    subroutine ajusteH(V, M, h, tol, tipo)
        implicit none
        real(8), intent(in) :: V(:,:,:), M(:), tol
        real(8), intent(inout) :: h
        integer, intent(in) :: tipo
        real(8) :: error
        real(8), allocatable :: E(:,:,:)
        integer :: d, n

        d = size(V, dim = 2)
        n = size(V, dim = 3)
        allocate(E(2, d, n))
        do while(.true.)
            select case (tipo)
            case (1)
                E = EulerSp(V, M, h) - EulerSP(EulerSP(V, M, h/2), M, h/2)
            case (2)
                E = EulerMejorSp(V, M, h) - EulerMejorSp(EulerMejorSp(V, M, h/2), M, h/2)
            case (3)
                E = rk4SP(V, M, h) - rk4SP(rk4SP(V, M, h/2), M, h/2)
            case (4)
                E = rkfSP(V, M, h) - rkfSP(rkfSP(V, M, h/2), M, h/2)
            end select
            error = maxval(abs(E))
            if(error>=tol) then
                h = h/2.0
            else
                if(1e2*error<tol) then ! con esto a veces lo aumenta a si que es razonable
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
        character(len=20) :: str
        integer, intent(in) :: k
        write (str, *) k
        str = adjustl(str)
    end function str

    subroutine scriptGnuplot(nMasas, d)
        implicit none
        integer, intent(in) :: nMasas, d
        integer :: i

        open(9, file = "sgnptMasas.p", status = "REPLACE")

        write(9,"(A)")"set nokey"
        if (d == 3) then
            write(9,"(A)")"set view equal xyz"
            write(9,"(A)")"splot \"
        else
            write(9,"(A)")"set size ratio -1"
            write(9,"(A)")"plot \"
        end if


        write(9, "(A)")"'fort."//trim(str(11))//"' with points lw 3,\" ! sino ni lo grafica
        do i = 2, nMasas-1
            write(9, "(A)")"'fort."//trim(str(i+10))//"' with lines lw 3,\"
        end do 
        write(9, "(A)")"'fort."//trim(str(nMasas+10))//"' with lines lw 3"

        close(9)
    end subroutine scriptGnuplot

    subroutine Solucion(Vi, M, h, tfinal, hModif, tol, tipo)
        implicit none
        logical, intent(in) :: hModif
        real(8), intent(in) :: tfinal, tol, Vi(:,:,:), M(:)
        real(8), intent(inout) :: h
        real(8), allocatable :: V(:,:,:)
        integer, intent(in) :: tipo
        real(8) :: t
        integer :: d, n, i

        t = 0 ! el tiempo lo llevamos como variable local solo para verificar t < tfinal
        d = size(V, dim = 2)
        n = size(Vi, dim = 3)
        allocate(V(2, d, n))

        do i = 1, n
            open(i+10)
        end do
        open(99, file = "h.dat")
        
        do i = 1, n
            write (i+10, *) Vi(1,:,i)
        end do
        V = Vi
        do while (t < tfinal)
            if (hModif) then
                call ajusteH(V, M, h, tol, tipo)
                write(99, *) h
            end if
            V = MetodosSP(V, M, h, tipo)
            t = t + h
            do i = 1, n
                write (i+10, *) V(1,:,i)
            end do
        end do

        close(99)
        do i = 1, n
            close(i+10)
        end do
        
        deallocate(V)
    end subroutine Solucion
end module edo
