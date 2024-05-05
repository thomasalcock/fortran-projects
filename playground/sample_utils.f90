module sample_utils

    implicit none

    public :: print_array, random_array, &
        random_number, bubble_sort, unique_ints, &
        count_unique_ints

    interface print_array
        module procedure :: print_int_array
        module procedure :: print_real_array
    end interface

    interface random_array
        module procedure :: random_real_array
        module procedure :: random_int_array 
    end interface

    interface random_number
        module procedure :: random_int
        module procedure :: random_real
    end interface

    interface bubble_sort
        module procedure :: bubble_sort_real_array
        module procedure :: bubble_sort_integer_array
    end interface

    ! interface unique_values
    !     module procedure :: unique_ints
    ! end interface

    contains
        integer function random_int(min, max) result(an_int)
            real :: real_between_0_and_1
            integer :: min, max
            call random_number(real_between_0_and_1)
            an_int = min + floor((max+1-min)*real_between_0_and_1)
        end function random_int

        real function random_real(min, max) result(a_real)
            real :: real_between_0_and_1
            real :: min, max
            call random_number(real_between_0_and_1)
            a_real = min + (max-min) * real_between_0_and_1
        end function random_real

        function random_real_array(min, max, n_elements) result(array_of_reals)
            real :: array_of_reals(n_elements), real_between_0_and_1(n_elements)
            real, intent(in) :: min, max
            integer, intent(in) :: n_elements
            call random_number(real_between_0_and_1)
            array_of_reals = min + (max-min) * real_between_0_and_1
        end function random_real_array
        
        function random_int_array(min, max, n_elements) result(array_of_ints)
            integer :: array_of_ints(n_elements)
            real :: reals_between_0_and_1(n_elements)
            integer, intent(in) :: min, max, n_elements
            call random_number(reals_between_0_and_1)
            array_of_ints = int((reals_between_0_and_1 * (max - min + 1)) + min)
        end function

        subroutine print_int_array(int_arr)
            integer, intent(in) :: int_arr(:)
            print *, int_arr
        end subroutine

        subroutine print_real_array(real_arr)
            real, intent(in) :: real_arr(:)
            print *, real_arr
        end subroutine

        subroutine bubble_sort_integer_array(x)
            integer, intent(inout) :: x(:)
            integer :: left, right, next_index, i
            logical :: swap_flag
            swap_flag = .true.
            do while(swap_flag)
                swap_flag = .false.
                do i = 1, size(x)-1
                    next_index = i+1
                    left = x(i)
                    right = x(next_index)
                    if (left > right) then
                        swap_flag = .true.
                        x(i) = right
                        x(next_index) = left
                    end if
                end do
            end do
        end subroutine bubble_sort_integer_array

        subroutine bubble_sort_real_array(x)
            real, intent(inout) :: x(:)
            real :: left, right
            integer :: next_index, i
            logical :: swap_flag
            swap_flag = .true.
            do while(swap_flag)
                swap_flag = .false.
                do i = 1, size(x)-1
                    next_index = i+1
                    left = x(i)
                    right = x(next_index)
                    if (left > right) then
                        swap_flag = .true.
                        x(i) = right
                        x(next_index) = left
                    end if
                end do
            end do
        end subroutine bubble_sort_real_array

        function count_unique_ints(array_of_ints) result(count)
            integer :: array_of_ints(:)
            integer :: i, count
            count = 1
            call bubble_sort_integer_array(array_of_ints)
            do i = 2, size(array_of_ints)
                if (array_of_ints(i-1) /= array_of_ints(i)) then
                    count = count + 1
                end if
            end do
        end function count_unique_ints

        function unique_ints(array_of_ints) result(unique_values)
            integer :: array_of_ints(:)
            integer, allocatable :: unique_values(:)
            integer :: i, n_unique_elements, unique_count

            n_unique_elements = count_unique_ints(array_of_ints)
            call bubble_sort_integer_array(array_of_ints)
            allocate(unique_values(n_unique_elements))
            
            unique_count = 1
            unique_values(1) = array_of_ints(1)

            do i = 2, size(array_of_ints)
                if (array_of_ints(i) /= unique_values(i-1)) then
                    unique_count = unique_count + 1
                    unique_values(unique_count) = array_of_ints(i)
                end if
            end do
        end function unique_ints
        

        
end module