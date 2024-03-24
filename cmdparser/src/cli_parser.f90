module cli_parser
    implicit none
    
    contains

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
        end subroutine 
end module