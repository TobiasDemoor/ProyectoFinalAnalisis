module edo
contains
    function Vprima(V)
        implicit none
        real(8), dimension(0:4) :: Vprima, V
        ! esto es relleno asi no se queja
        real(8), parameter :: L1 = 10, L2 = 20, R = 1000, C = 0.001

        Vprima(0) = 1.0
        Vprima(1) = V(2)
        Vprima(2) = (sin(3*V(0)) + (V(3) - V(1)) / C) / L1
        Vprima(3) = V(4)
        Vprima(4) = ((V(1) - V(3)) / C - V(4) * R) / L2
        ! ---------------------------------
    end function Vprima

    function rk4SP(V, h)
        real(8), dimension(:) :: V
        real(8), allocatable, dimension(:)  :: rk4SP, K1, K2, K3, K4
        real(8) :: h
        integer :: orden

        orden = size(V)
        allocate(rk4SP(0:orden-1), K1(0:orden-1), K2(0:orden-1), K3(0:orden-1), K4(0:orden-1))
        K1 = h*Vprima(V)
        K2 = h*Vprima(V+K1/2.0)
        K3 = h*Vprima(V+K2/2.0)
        K4 = h*Vprima(V+K3)
        rk4SP = V + (K1 + 2.0*K2 + 2.0*K3 +K4)/6.0
        deallocate(K1, K2, K3, K4)
    end function

    subroutine rk4(Vi,  h, max, unit, format, hModif, tol)
        implicit none
        logical,intent(in) :: hModif
        integer, intent(in) :: unit
        real(8), intent(in) :: max, tol, Vi(:)
        real(8), intent(inout) :: h
        character (len=8), intent(in):: format
        real(8), allocatable, dimension(:)  :: V
        integer :: orden

        orden = size(Vi)
        allocate(V(0:orden-1))
        open(3, file = "h.txt")
        write(unit, format) Vi
        V = Vi
        do while (V(0) <= max)
            if (hModif) then
                write (3, '(F15.7)') h
                call est1(V, h, tol, 3)
            end if
            V = rk4SP(V, h)
            write (unit, format) V
        end do
        close(3)
        deallocate(V)
    end subroutine rk4

end module edo