module fortran_utils
    use iso_fortran_env, only: int32, real32, character_kinds

    implicit none

    contains

        character(20) function int_to_char(k) result(str)
            integer, intent(in) :: k
            write (str, *) k
            str = adjustl(str)
        end function int_to_char

        subroutine print_matrix(matrix)
            real(kind=real32), intent(in) :: matrix(:,:)
            integer :: i, j, ncol, nrow
            character(len=10) :: format_str

            nrow = size(matrix, dim=1)
            ncol = size(matrix, dim=2)
            
            format_str = '(' // trim(int_to_char(ncol)) // 'F6.2)'
            
            do i = 1, nrow
                write(*, format_str, advance='no') (matrix(i, j), j = 1, ncol)
                print *
            end do
            print *, ""
        end subroutine

        subroutine get_keys_and_values(input_args, keys, values)
            character(len=32), allocatable, intent(in) :: input_args(:)
            character(len=32), allocatable, intent(out) :: keys(:)
            character(len=32), allocatable, intent(out) :: values(:)
            integer :: i, key_index, value_index, n_args
            i = 0
            key_index = 0
            value_index = 0
            n_args = size(input_args) 

            allocate(keys(n_args))
            allocate(values(n_args))
            
            if (.not. allocated(keys) .or. .not. allocated(values)) then
                error stop "Error during allocation for keys and values"
            end if

            do i = 1, n_args
                if (mod(i, 2) /= 0) then
                    key_index = key_index + 1
                    keys(key_index) = input_args(i)
                else 
                    value_index = value_index + 1
                    values(value_index) = input_args(i)
                end if
            end do
        end subroutine

        subroutine parse_input_args(input_args, filepath, number)
            character(len=32), allocatable, intent(in) :: input_args(:)
            character(len=65), parameter :: error_message = "usage: ./cli_test --filepath <path to file> -n <positive integer>"
            integer(kind=int32) :: err, i
            
            integer(kind=int32) :: number
            character(len=32), intent(out) :: filepath
        
            do i = 1, size(input_args), 2
                if (input_args(i) == "-f") then 
                    if (i + 1 <= size(input_args)) then
                        read(input_args(i+1), *, iostat=err) filepath
                        if (err /= 0)  then
                            error stop "Error reading -f / --filepath"
                        end if
                    end if
                else if (input_args(i) == "-n") then
                    if (i + 1 <= size(input_args)) then
                        read(input_args(i+1),*, iostat=err) number
                        if (err /= 0)  then
                            error stop "Error reading -n / --number"
                        end if
                    end if
                else
                    error stop error_message
                end if
            end do
        end subroutine

        subroutine print_args(args)
            integer :: i
            character(len=32), intent(in) :: args(:)
            do i = 1, size(args)
                print *, args(i)
            end do
        end subroutine print_args

        subroutine get_command_line_arguments(args)
            integer :: n_args
            character(len=32) :: arg
            character(len=32), allocatable :: args(:)
            integer :: arg_index

            n_args = command_argument_count()
            allocate(args(n_args))
            
            arg_index = 1
            do 
                call get_command_argument(arg_index, arg)
                if (len_trim(arg) == 0) then
                    exit
                else if (len_trim(arg) >= 32) then
                    print *, "Error:", arg, " exceeds the maximum length of 32 characters for an argument!"
                    stop 1
                end if
                args(arg_index) = arg
                arg_index = arg_index + 1
            end do

            !deallocate(args)
        end subroutine 
end module