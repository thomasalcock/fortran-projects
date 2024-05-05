program sampling_procedures
    use sample_utils
    implicit none
    integer :: x(10)
    real :: y(10)
    
    x = random_array(1, 10, 10)
    y = random_array(1.0, 2.0, 10)
    
    call print_array(x)
    call print_array(y)

    call bubble_sort(x)
    call bubble_sort(y)

    call print_array(x)
    call print_array(y)
    
end program