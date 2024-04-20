program line_search
    implicit none
    real, allocatable :: search_values(:)
    integer :: i
    real :: x, y
    search_values = sequence(1.0, 1.5, 0.005)
    do i = 1, size(search_values)
        x = search_values(i)
        y = x**2 - 2
        if (abs(y) < 0.01) then
            print *, "x=", x, "y=", y
            stop
        end if
    end do

    contains
        function sequence(min, max, step_size) result(values)
            real :: min, max, step_size
            real, allocatable :: values(:)
            integer :: n_steps, i
            n_steps = 0
            i = 0
            if (max <= min) then
                error stop "max must be greater than min!"
            end if
            n_steps = int((max - min) / step_size)
            allocate(values(n_steps))
            values(1) = min
            do i = 2, (n_steps - 1)
                values(i) = values(i-1) + step_size
            end do
            values(n_steps) = max
        end function sequence

end program line_search