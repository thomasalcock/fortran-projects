program fortan_utils_test
    
    use iso_fortran_env, only: int32, real32
    use fortran_utils

    implicit none

    real(kind=real32) :: A(2,3), B(3,4), result(2,4)
    !character(len=32), allocatable :: inputs(:)
    ! character(len=32) :: filepath
    ! integer(kind=int32) :: number
    
    
    ! character(len=32), allocatable :: keys(:), values(:)
    ! character(len=32) :: expected_keys(2) = [character(len=32) :: "--filepath",  "--number"]
    ! character(len=32) :: expected_types(2) = [character(len=32) :: "string", "integer"]

    call random_number(A)
    call random_number(B)
    
    result = matmul(A, B)

    call print_matrix(A)
    call print_matrix(B)
    call print_matrix(result)

    

end program