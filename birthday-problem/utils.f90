module utils

    public :: has_duplicates, contains_duplicates, runif, random_integers

    contains

        function simulate_birthday_problem(n_birthdays) result(prob_same_birthday)
            integer :: n_simulations = 1000
            integer, intent(in) :: n_birthdays
            integer :: birthdays(n_birthdays)
            logical :: at_least_two_same_birthdays
            integer :: i
            integer :: same_birthday_counter
            real :: prob_same_birthday

            ! when declaring and initializing a variable at the same time
            ! the variable acquires a "save" attribute, so that
            ! it its value persists across function calls.
            ! to turn this unintuitive behaviour off, variable declaration
            ! must happen independently from initialization
            same_birthday_counter = 0 
            
            do i = 1, n_simulations
                birthdays = random_integers(n_birthdays, 1, 365)
                at_least_two_same_birthdays = has_duplicates(birthdays)
                if (at_least_two_same_birthdays) then
                    same_birthday_counter = same_birthday_counter + 1
                end if
            end do
            prob_same_birthday = real(same_birthday_counter) / n_simulations
        end function simulate_birthday_problem

        function has_duplicates(array) result(any_duplicates)
            integer, intent(in) :: array(:)
            integer :: i
            integer :: n_duplicates
            logical :: any_duplicates

            any_duplicates = .false.

            do i = 1, size(array)
                n_duplicates = count(array  == array(i))
                any_duplicates = (n_duplicates > 1)
                if (any_duplicates) then
                    exit
                end if
            end do
        end function has_duplicates

        function runif(n) result(numbers)
            implicit none
            integer, intent(in) :: n
            real, dimension(n) :: numbers
            call random_number(numbers)
        end function runif

        function random_integers(n, lower_bound, upper_bound) result(numbers)
            implicit none
            integer, intent(in) :: n, lower_bound, upper_bound
            real, dimension(n) :: unif_numbers
            integer, dimension(n) :: numbers
            
            unif_numbers = runif(n)
            numbers = int((unif_numbers * (upper_bound - lower_bound + 1)) + lower_bound)
        end function

end module utils