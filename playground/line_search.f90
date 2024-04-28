program line_search
    implicit none
    real, parameter :: pi = 4.0 * atan(1.0)
    real, parameter :: e = 2.71
    real, allocatable :: vals(:), vals2(:)
    
    vals = sequence(1.0, 1.5, 0.005)
    vals2 = sequence(-2.0, 2.0, 0.1)
    
    print *, "sqrt(2) = ", do_line_search(vals, sqrt2)
    print *, "min(1 - x**2) =", do_line_search(vals2, valley)
    print *, "ackley =", do_line_search(vals2, ackley)

    contains

        real function sqrt2 (a) result(b)
            real :: a
            b = a**2 - 2
        end function sqrt2

        real function valley(input) result(output)
            real :: input
            output = (1 - input**2)
        end function valley

        real function ackley(x) result(y)
            real :: x
            y = -20.0 * exp(0.2 * x) - exp(cos(2 * pi * x)) + 20 + e        
        end function ackley

        !real function grid_search(a, b, func) result()

        real function do_line_search(search_values, func) result(x)
            real, intent(in), allocatable :: search_values(:)
            interface
                real function func(z) result(y)
                    real :: z
                end function func
            end interface
            integer :: i
            real :: y
            do i = 1, size(search_values)
                x = search_values(i)
                y = func(x)
                if (abs(y) < 0.01) then
                    exit                    
                end if
            end do
        end function do_line_search

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