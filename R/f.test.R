f.test <- function (formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) 
{
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  METHOD <- "Fisher's Test"

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
  
  if (is.list(y)) {
    if (length(y) < dp[[2L]])
      stop("'y' must be a list with at least 2 elements") 
  }
  
  n <- length(y)
  x.levels <- levels(factor(group))
  k <- length(x.levels)

  ni<- tapply(y, group, length)
  
  
  vars <- tapply(y, group, var)
  vars.max <- max(vars)
  vars.min <- min(vars)
  
  F.test<- vars.max/vars.min 
  
  df1<- ni[which.max(vars)]-1
  df2<- ni[which.min(vars)]-1
  
  p.value <- pf(F.test, df1, df2, lower.tail = F)
  

  
  if (verbose) {
    cat("\n", "", METHOD, paste("(alpha = ", alpha, ")", 
                                sep = ""), "\n", sep = " ")
    cat("-------------------------------------------------------------", 
        "\n", sep = " ")
    cat("  data :", DNAME, "\n\n", sep = " ")
    cat("  statistic   :", F.test, "\n", sep = " ")
    cat("  df1         :", df1, "\n", sep = " ")
    cat("  df2         :", df2, "\n", sep = " ")
    
    cat("  p.value     :", p.value, "\n\n", sep = " ")
    cat(if (p.value > alpha) {
      "  Result      : Variances are homogeneous."
    }
    else {
      "  Result      : Variances are not homogeneous."
    }, "\n")
    cat("-------------------------------------------------------------", 
        "\n\n", sep = " ")
  }
  result <- list()
  result$statistic <- F.test
  result$parameter <- c(df1,df2)
  result$p.value <- p.value
  result$alpha <- alpha
  result$method <- METHOD
  result$data <- data
  result$formula <- formula
  invisible(result)
}
