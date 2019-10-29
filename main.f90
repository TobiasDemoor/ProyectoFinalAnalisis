program main
    use edo
    implicit none
    real(8), dimension(0:3,3,2) :: Vi
    real(8), dimension(2) :: M
    real(8) :: tfinal, h, tol = 0

    M = [1,1]
    Vi(0,:,:) = 0
    Vi(3,:,:) = 0
    Vi(1,:,1) = 1
    Vi(1,:,2) = -1
    Vi(2,:,1) = 1
    Vi(2,:,2) = -1
    h = 0.1
    tfinal = 1
    call rk4(Vi, M, h, tfinal, .False., tol)
end program main