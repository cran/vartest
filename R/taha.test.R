taha.test<-function (formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) 
{
  
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  METHOD <- "Taha Test"
  
  if (na.rm) {
    completeObs <- complete.cases(data)
    data <- data[completeObs, ]
  }
  if (any(colnames(data) == dp[[3L]]) == FALSE) 
    stop("The name of group variable does not match the variable names in the data. The group variable must be one factor.")
  if (any(colnames(data) == dp[[2L]]) == FALSE) 
    stop("The name of response variable does not match the variable names in the data.")
  
  y = data[[dp[[2L]]]] 
  order<- order(y)
  y<- y[order]
  group = data[[dp[[3L]]]]
  group<- group[order]
  
  if (!(is.factor(group) | is.character(group))) 
    stop("The group variable must be a factor or a character.")
  if (is.character(group)) 
    group <- as.factor(group)
  if (!is.numeric(y)) 
    stop("The response must be a numeric variable.")
  
  
  n <- length(y)
  x.levels <- levels(factor(group))
  k <- length(x.levels)
  ni<- tapply(y, group, length)
  rank<- rank(y)
  ani<- rank^2
  a.ort<- (1/n)*sum(ani)  
  v.square<- (1/(n-1))* sum((ani-a.ort)^2)
  Ai<- tapply(ani, group, mean)
  
  
  THtest<- sum(ni*((Ai-a.ort)^2)/v.square)
  
  df<- k-1
  p.value<- pchisq(THtest, df, lower.tail = F)
  
  if (verbose) {
    cat("\n", "", METHOD, paste("(alpha = ", alpha, ")", 
                                sep = ""), "\n", sep = " ")
    cat("-------------------------------------------------------------", 
        "\n", sep = " ")
    cat("  data :", DNAME, "\n\n", sep = " ")
    cat("  statistic  :", THtest, "\n", sep = " ")
    cat("  df         :", df, "\n", sep = " ")
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
  result$statistic <- THtest
  result$parameter <- df
  result$p.value <- p.value
  result$alpha <- alpha
  result$method <- METHOD
  result$data <- data
  result$formula <- formula
  invisible(result)
}