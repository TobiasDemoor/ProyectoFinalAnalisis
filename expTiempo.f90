program main
    use edo
    implicit none
    real(8), allocatable :: Vi(:,:,:), M(:)
    real(8) :: tfinal, h, tol = 1
    real(8) :: inicio, fin, tiempo(4,5)
    integer :: i, j

    open(2, file = 'sistSolar.dat')
    call LeeMasas(Vi, M, 2, 3)

    tfinal = 30*24*3600

    do i = 1, 4
        do j = 1, 5
            h = 2
            call cpu_time(inicio)
            call Solucion(Vi, M, h, tfinal, .True., tol, i)
            call cpu_time(fin)
            tiempo(i,j) = fin-inicio
        end do
    end do
    open (2, file = 'tiempos.txt')
    do i = 1, 4
        write(2, *) i
        write(2, *) tiempo(i,:)
        write(2, *) 'Promedio: ', sum(tiempo(i,:))/5.0
    end do
    close(2)
end program main