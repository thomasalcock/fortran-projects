program sampling_procedures
    use sample_utils
    implicit none
    integer :: x(10)
    integer, allocatable :: x_unique(:)

    
    x = random_array(4, 18, 10)
    call print_array(x)
    x_unique = unique_ints(x)

    call bubble_sort(x_unique)
    call print_array(x_unique)
end program