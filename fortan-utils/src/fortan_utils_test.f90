program fortan_utils_test
    
    use iso_fortran_env, only: int32, real32
    use fortran_utils

    implicit none

    real(kind=real32) :: A(2,3), B(3,4), result(2,4)
    character(len=32), allocatable :: inputs(:)
    character(len=32) :: filepath
    integer(kind=int32) :: number

    character(len=32), allocatable :: keys(:), values(:)

    call random_number(A)
    call random_number(B)
    
    result = matmul(A, B)

    ! test matrix printing function
    call print_matrix(A)
    call print_matrix(B)
    call print_matrix(result)

    ! test cli parser
    call get_command_line_arguments(inputs)
    call get_keys_and_values(inputs, keys, values)
    print *, "keys: ", keys
    print *, "values: ", values
 
    !call parse_input_args(inputs, filepath, number)
    
    print *, "inputs: ", inputs
    print *, "filepath: ", filepath
    print *, "number: ", number
 
end program