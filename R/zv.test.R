zv.test <- function (formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE){
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  METHOD <- "Z Variance Test"
  
  if (na.rm) {
    completeObs <- complete.cases(data)
    data <- data[completeObs, ]
  }
  if (any(colnames(data) == dp[[3L]]) == FALSE) 
    stop("The name of group variable does not match the variable names in the data. The group variable must be one factor.")
  if (any(colnames(data) == dp[[2L]]) == FALSE) 
    stop("The name of response variable does not match the variable names in the data.")
  
  y = data[[dp[[2L]]]]
  group = data[[dp[[3L]]]]
  
  if (!(is.factor(group) | is.character(group))) 
    stop("The group variable must be a factor or a character.")
  if (is.character(group)) 
    group <- as.factor(group)
  if (!is.numeric(y)) 
    stop("The response must be a numeric variable.")
  
  n <- length(y)
  x.levels <- levels(factor(group))
  k <- length(x.levels)
  
  y.variance <- tapply(y, group, var)
  
  ni<- tapply(y, group, length)
  
  ci <- 2+(1/ni)
  
  y.means <- tapply(y, group, mean)
  
  z<- list()
  for (i in x.levels) {
    z[[i]]<- sum((y[group==i]-y.means[i])^2) 
  }
  mse_nom <- sum(unlist(z)) 
  
  MSE<- mse_nom/(n-k)
  
  zi_left<- sqrt((ci*(ni-1)*y.variance)/MSE)
  zi_right<- sqrt(ci*(ni-1)-(ci/2)) 

  zi<- as.numeric(zi_left - zi_right)

  vtest<- sum(zi^2)/k-1

  df1 = k-1
  df2 = Inf
  
  p.value = pf(vtest, df1, df2, lower.tail = F)

if (verbose) {
  cat("\n", "", METHOD, paste("(alpha = ", alpha, ")", 
                              sep = ""), "\n", sep = " ")
  cat("-------------------------------------------------------------", 
      "\n", sep = " ")
  cat("  data :", DNAME, "\n\n", sep = " ")
  cat("  statistic  :", vtest, "\n", sep = " ")
  cat("  num df     :", df1, "\n", sep = " ")
  cat("  denom df   :", df2, "\n", sep = " ")
  cat("  p.value    :", p.value, "\n\n", sep = " ")
  cat(if (p.value > alpha) {
    "  Result     : Variances are homogeneous."
  }
  else {
    "  Result     : Variances are not homogeneous."
  }, "\n")
  cat("-------------------------------------------------------------", 
      "\n\n", sep = " ")
}
result <- list()
result$statistic <- vtest
result$parameter <- c(df1, df2)
result$p.value <- p.value
result$alpha <- alpha
result$method <- METHOD
result$data <- data
result$formula <- formula
invisible(result)
}
