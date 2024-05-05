program sampling_procedures
    use sample_utils
    implicit none
    integer :: x(10)
    integer, allocatable :: x_unique(:)
    
    x = random_array(1, 10, 10)
    call bubble_sort(x)
    call print_array(x)
    print *, unique_ints(x)
end program