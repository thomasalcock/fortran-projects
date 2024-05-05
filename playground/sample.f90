program sampling_procedures
    use sample_utils
    implicit none
    integer :: x(10)
    integer, allocatable :: x_unique(:)
    real :: y(10)
    real, allocatable :: y_unique(:)

    x = random_array(4, 18, 10)
    y  = random_array(2.4, 28.2, 10)
    
    x_unique = unique(x)
    y_unique = unique(y)

    call print_array(x)
    call print_array(x_unique)

    call print_array(y)
    call print_array(y_unique)
end program