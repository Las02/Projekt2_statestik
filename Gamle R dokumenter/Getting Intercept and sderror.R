

ic <- 8.1834
ze.teb <- -1.9534
not <- -2.5031

A <- cbind(diag(3))
A[2,1] <- 1
A[3,1] <- 1

# Getting the intercept
A %*% c(ic,ze.teb,not)

# And the std. error