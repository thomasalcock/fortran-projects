program command_line_parser
    
    use iso_fortran_env, only: int32, character_kinds
    use cli_parser

    implicit none

    integer(kind=int32) :: number, str_conversion_status
    character(len=32), allocatable :: inputs(:)
    character(len=65), parameter :: error_message = "usage: ./cli_test --filepath <path to file> -n <positive integer>"
    
    call get_command_line_arguments(inputs)
    if ((inputs(1) /= "--filepath")) then
        print *, error_message
        stop 1
    else if (inputs(3) /= "-n") then
        print *, error_message
        stop 1
    end if
    read(inputs(4),*, iostat=str_conversion_status) int32

end program