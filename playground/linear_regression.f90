program linear_regression_gd
    ! Linear regression using gradient descent

    implicit none
    
    integer n, i
    real :: x(100000, 3)
    real :: y(100000, 1), y_hat(100000, 1), residuals(100000, 1), predictions(100000, 1)
    real :: coefs(3, 1), coefs_est(3, 1), diffs(3, 1), grads(3, 1)
    real :: cost, rmse, mse, learning_rate, threshold
    
    coefs(1,1) = 1.337
    coefs(2,1) = 2.3445
    coefs(3,1) = 3.1415

    threshold = 0.0001
    n = size(x)
    learning_rate = 0.05
    i = 0
    do i = 1, n
        x(i, 1) = 1.0
        x(i, 2) = rand()
        x(i, 3) = rand()
    end do

    call random_number(coefs_est)    
    diffs = abs(coefs_est - coefs)
    i = 0
    print *, "coefs_est=", coefs_est
    do while (any(diffs > 0))
        y_hat = matmul(x, coefs_est)
        residuals = y - y_hat
        cost = sum(residuals**2)
        grads = -2 * matmul(transpose(x), residuals)
        coefs_est = coefs_est - learning_rate * grads
        diffs = abs(coefs - coefs_est)
        i = i + 1
        print *, "iteration=", i, "gradients=", grads, "cost=", cost, "coefs_est=", coefs_est
    end do

    predictions = matmul(x, coefs_est)
    mse = sum(predictions - y)**2 / n
    rmse = sqrt(mse)
    print *,"RMSE = ", rmse, "MSE = ", mse

end program