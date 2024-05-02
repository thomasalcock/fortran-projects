program probability_distributions
    implicit none
    
    integer :: i
    real, allocatable :: exp_samples(:), pareto_samples(:)
    
    exp_samples = sample_exponential(5, 1.2)
    pareto_samples = sample_pareto(5, 1.0, 0.5)
    
    do i = 1, size(exp_samples)
        write(*,*) exp_samples(i), pareto_samples(i)
    end do

    contains
        function sample_exponential(n, lambda) result(x)
            real, allocatable :: x(:)
            real :: lambda
            integer :: n
            allocate(x(n))
            call random_number(x)
            x = -log(1 - x) / lambda
        end function sample_exponential

        function sample_pareto(n, x_min, alpha) result(x)
            real, allocatable :: x(:)
            real :: x_min, alpha
            integer :: n
            allocate(x(n))
            call random_number(x)
            x = x_min * (1 - x)**(-1/alpha)
        end function sample_pareto
        
end program