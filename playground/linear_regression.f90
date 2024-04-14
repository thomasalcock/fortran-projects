program linear_regression_gd
    ! Linear regression using gradient descent

    implicit none
    
    integer n, i
    real :: x(100000)
    real :: y(100000), predictions(100000), temp(100000)
    real :: beta, alpha, beta_est, alpha_est, cost, rmse, mse, grad_alpha, grad_beta, learning_rate
    real :: beta_diff, alpha_diff, threshold
    
    beta = 1.337
    alpha = 0.420
    threshold = 0.0001
    n = size(x)
    learning_rate = 0.5

    call random_number(x)
    y = alpha + beta * x

    call random_number(beta_est)
    call random_number(alpha_est)
    
    beta_diff = abs(beta - beta_est)
    alpha_diff = abs(alpha - alpha_est)

    do while (beta_diff > threshold .or. alpha_diff > threshold)
        
        temp = alpha_est + beta_est * x - y
        cost = (1.0 / 2 * n) * sum(temp**2)
        
        grad_alpha = (1.0 / n) * sum(temp)
        grad_beta = (1.0 / n) * sum(temp * x)
        
        alpha_est = alpha_est - learning_rate * grad_alpha
        beta_est = beta_est - learning_rate * grad_beta
        
        beta_diff = abs(beta - beta_est)
        alpha_diff = abs(alpha - alpha_est)
        i = i + 1

        print *, "iteration = ", i, "cost = ", cost, "alpha = ", alpha_est, "beta = ", beta_est
    end do
    predictions = alpha_est + beta_est * x
    
    mse = sum(predictions - y)**2 / n
    rmse = sqrt(mse)

    print *,"RMSE = ", rmse, "MSE = ", mse

end program