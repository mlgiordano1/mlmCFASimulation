# Generic covariance matrix computation (compare to the 'cov()' function)
covMat <- function(matrix) {
  # number of subjects
  n <- nrow(matrix) 
  # means (might be a better way to do this)
  means <- t(matrix(rep(colMeans(matrix), n), ncol=n))
  # difference matrix
  d <- matrix - means
  #covariance matrix
  return((1/(n-1)) * t(d) %*% d)
}

# test with a very simple toy set
ex <- matrix(data = c(4.0, 4.2, 3.9, 4.3, 4.1, 
                      2.0, 2.1, 2.0, 2.1, 2.2, 
                      0.60, 0.59, 0.58, 0.62, 0.63),
             nrow = 5)
covMat(ex)
cov(ex)


# So a decent approach will be to program a function to compute.
# but then also go through it and do some by hand computations
# I think I need to create a matrix with group means, use that as the difference matrix
# and that might do it

# muthen estimator

