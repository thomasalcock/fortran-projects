program command_line_parser
    
    use iso_fortran_env, only: int32, character_kinds
    use cli_parser

    implicit none

    integer(kind=int32) :: number, err
    integer(kind=int32) :: i = 0
    character(len=32) :: filepath
    
    character(len=32), allocatable :: inputs(:)

    ! TODO: abstract thse parameters names into a struct or something that can be used to generate this usage
    ! message as well as the 
    character(len=65), parameter :: error_message = "usage: ./cli_test --filepath <path to file> -n <positive integer>"
    
    call get_command_line_arguments(inputs)
    
    do i = 1, size(inputs), 2
        select case (inputs(i))
            case ("--filepath", "-f")
                if (i + 1 <= size(inputs)) then
                    read(inputs(i+1), *, iostat=err) filepath
                    if (err /= 0)  then
                        error stop "Error reading -f / --filepath"
                    end if
                end if
            case ("--number", "-n")
                if (i + 1 <= size(inputs)) then
                    read(inputs(i+1),*, iostat=err) number
                    if (err /= 0)  then
                        error stop "Error reading -n / --number"
                    end if    
                end if
            case default
                error stop error_message
        end select
    end do

    print *, "filepath: ", filepath
    print *, "n + 23: ", number + 23

end program