program birthdayproblem
    use utils, only: simulate_birthday_problem
    implicit none
    integer :: j
    integer, dimension(50) :: n_people = [(j, j = 5, 54)]
    integer :: i
    do i = 1, size(n_people)
        print *, i, ",", simulate_birthday_problem(n_people(i))
    end do

end program