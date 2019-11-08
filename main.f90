program main
    use edo
    implicit none
    real(8), parameter :: MTierra = 5.9736e24 ! lo dejo aca porque puede ser útil
    real(8), allocatable :: Vi(:,:,:), M(:)
    ! en escalas astronomicas 1m es nada, asi que ndeah
    real(8) :: tfinal, h, tol = 1

    open(2, file = 'sistSolar.dat')
    call LeeMasas(Vi, M, 2, 2) ! en 2 dimensiones se ve mas lindo
    ! V(posicion/velocidad, x/y/z, masas)
    ! allocate(Vi(2,3,2),M(2))
    ! M = [dble(5e13), dble(1e11)]
    ! Vi(1,:,:) = 0 ! pos xyz de ambas masas en 0
    ! Vi(2,:,:) = 0 ! vel xyz de ambas masas en 0
    ! ! masa 2
    ! Vi(1,1,2) = 50
    ! Vi(2,2,2) = 10
    ! Vi(2,3,2) = 3
    ! ejecute con ajuste de h y vi que dentro de todo para el ejemplo que esta aca
    ! este es el mejor valor para todo t, igual con ajuste de h es mejor
    h = 2
    ! tfinal = 27*24*3600 ! el período orbital de la luna es de 27 dias
    tfinal = 3650*24*3600 ! 10 años para ver orbitas externas
    call rk4(Vi, M, h, tfinal, .True., tol)
    call scriptGnuplot(size(Vi, dim = 3), size(Vi, dim = 2))
    call system("gnuplot -persist 'sgnptMasas.p'")
end program main