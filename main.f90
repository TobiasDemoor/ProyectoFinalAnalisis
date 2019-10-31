program main
    use edo
    implicit none
    real(8), dimension(3,2,2) :: Vi
    real(8), dimension(2) :: M
    real(8) :: tfinal, h, tol = 0

    M = [dble(1e10), dble(1e10)]
    Vi(3,:,:) = 0 ! aceleracion inicial = 0
    Vi(1:2,:,1) = 0 ! masa 1 estatica en origen
    ! masa 2
    Vi(1,1,2) = 5
    Vi(2,:,2) = 0
    Vi(2,2,2) = 0.1
    Vi(2,2,1) = -0.1
    h = 0.1
    tfinal = 1000
    call rk4(Vi, M, h, tfinal, .False., tol)
end program main