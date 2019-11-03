program main
    use edo
    implicit none
    real(8), dimension(3,2,2) :: Vi
    real(8), dimension(2) :: M
    real(8) :: tfinal, h, tol = 0

    ! V(posicion/velocidad, x/y/z, masas)
    M = [dble(5e13), dble(1e11)]
    Vi(1,:,:) = 0 ! masa 1 estatica en origen
    ! masa 2
    Vi(1,1,2) = 50
    Vi(2,:,:) = 0
    Vi(2,2,2) = 10
    h = 0.1
    tfinal = 1000
    call rk4(Vi, M, h, tfinal, .False., tol)
    call scriptGnuplot(size(Vi, dim = 3))
end program main