program main
    use edo
    implicit none
    real(8), allocatable :: Vi(:,:,:), M(:)
    ! en escalas astronomicas 1m no es nada
    real(8) :: tfinal, h, tol = 1

    open(2, file = 'sistSolar.dat')
    call LeeMasas(Vi, M, 2, 3)

    h = 2
    ! tfinal = 30*3600 ! Deimos tiene un periodo orbital de ~30 horas y Fobos de ~7 horas
    ! tfinal = 27*24*3600 ! el período orbital de la luna es de ~27 dias
    tfinal = 3650*24*3600 ! 10 años para ver orbitas externas
    ! 1: euler simple
    ! 2: euler mejorado
    ! 3: rk4
    ! 4: rkf
    call Solucion(Vi, M, h, tfinal, .True., tol, 4)
    call scriptGnuplot(size(Vi, dim = 3), size(Vi, dim = 2))
    call system("gnuplot -persist 'sgnptMasas.p'")
end program main