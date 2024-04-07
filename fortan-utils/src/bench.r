A <- matrix(
    data = c(0.30, 0.28,  0.97, 0.54,  0.23,  0.14),
    ncol = 3,
    nrow = 2,
    byrow = T
)

B <- matrix(
    data = c(0.58, 0.57, 0.26, 0.66, 0.82, 0.01,0.42, 0.83, 0.54, 0.82, 0.43, 0.82),
    ncol = 4,
    nrow = 3,
    byrow = T
)

print(round(A %*% B, 2))