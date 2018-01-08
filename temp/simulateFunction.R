
# ToDO:
# Currently does not work with my fitting function
# Add arguments for the number of samples and the cluster sizes
# tweak until we get the right amount of ICC
# 
simMLCfa <- function(withinModel, 
                     betweenModel) {
  # generate within data
  w <- lavaan::simulateData(model = withinModel, 
                     model.type = "cfa", 
                     skewness = 0, 
                     ov.var = 10,
                     sample.nobs = 3000, 
    std.lv = FALSE)
  w$cluster <- rep(1:100, 30)
  # generate the between data
  b <- lavaan::simulateData(model = betweenModel, 
                     model.type = "cfa", 
                     skewness = 0, 
                     ov.var = 5,
                     sample.nobs = 100, 
    std.lv = FALSE)
  b$cluster <- 1:100
  # merge together
  d <- merge(w, b, by = "cluster")
  # brief manipulation (could probably clean this up)
  d$y1 <- d$y1.x + d$y1.y
  d$y2 <- d$y2.x + d$y2.y
  d$y3 <- d$y3.x + d$y3.y
  d$y4 <- d$y4.x + d$y4.y
  d$y5 <- d$y5.x + d$y5.y
  d$y6 <- d$y6.x + d$y6.y
  d$y7 <- d$y7.x + d$y7.y
  d$y8 <- d$y8.x + d$y8.y
  d$y9 <- d$y9.x + d$y9.y
  # subset
  d <- d[,c("cluster", paste("y", 1:9, sep = ""))]
  return(d)
}



