library(magrittr)

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
set.seed(1)
xGroup1 <- rnorm(n = 30, mean = 100, 2)
xGroup2 <- rnorm(n = 30, mean = 110, 2)
yGroup1 <- rnorm(n = 30, mean = 0, 2)
yGroup2 <- rnorm(n = 30, mean = 10, 2)

x <- c(xGroup1, xGroup2)
y <- c(yGroup1, yGroup2)

dat <- cbind(c(rep(1, 30), rep(2, 30)), x, y)
colnames(dat) <- c("id", "x", "y")
dat

datMn <- as.data.frame(dat) %>% 
  dplyr::group_by(id) %>%
  dplyr::summarise(x_mn = mean(x), y_mn = mean(y))
merge(dat, datMn, by = "id")


as.data.frame(dat) %>%
      #add_rownames()%>% #if the rownames are needed as a column
      dplyr::group_by(id) %>% 
      dplyr::mutate(x_c= x-mean(x), y_c = y-mean(y)) %>%
      dplyr::select(x_c, y_c)


# Generic covariance matrix computation (compare to the 'cov()' function)
muth_w_Mat <- function(matrix) {
  # number of subjects
  n <- nrow(matrix) 
  # means (might be a better way to do this)
  means <- t(matrix(rep(colMeans(matrix), n), ncol=n))
  # difference matrix
  d <- as.data.frame(dat) %>%
      dplyr::group_by(cluster) %>% 
      dplyr::mutate(x_c= x-mean(x), y_c = y-mean(y)) %>%
      dplyr::select(x_c, y_c)
  #covariance matrix
  return((1/(n-1)) * t(d) %*% d)
}
