program main
    use edo
    implicit none
    real(8), parameter :: MTierra = 5.9736e24 ! lo dejo aca porque puede ser útil
    real(8), allocatable :: Vi(:,:,:), M(:)
    ! en escalas astronomicas 1m es nada, asi que ndeah
    real(8) :: tfinal, h, tol = 1

    open(2, file = 'marte.dat')
    call LeeMasas(Vi, M, 2, 3) ! en 2 dimensiones se ve mas lindo
    ! ejecute con ajuste de h y vi que dentro de todo para el ejemplo que esta aca
    ! este es el mejor valor para todo t, igual con ajuste de h es mejor
    h = 2
    tfinal = 30*3600 ! Deimos tiene un periodo orbital de ~30 horas y Fobos de ~7 horas
    ! tfinal = 27*24*3600 ! el período orbital de la luna es de ~27 dias
    ! tfinal = 3650*24*3600 ! 10 años para ver orbitas externas
    call rk4(Vi, M, h, tfinal, .True., tol)
    call scriptGnuplot(size(Vi, dim = 3), size(Vi, dim = 2))
    call system("gnuplot -persist 'sgnptMasas.p'")
end program main