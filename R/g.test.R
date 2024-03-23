g.test <- function (formula, data,alpha = 0.05, na.rm = TRUE, verbose = TRUE) 
{
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  METHOD <- "G Test"
  
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
  ni <- tapply(y, group, length)
  vi <- ni-1
  vpool<- sum(vi)
  ni.mean<- mean(ni)
  
  vars <- tapply(y, group, var)
  vars.sum <- sum(vars)
  vars.max <- max(vars)
  vi.max <- ni[which.max(vars)]-1
  
  
  G <- (vi.max*vars.max)/sum(vi*vars) 
  
  f <- 1/(((1/G)-1)/(vpool/(vi.max-1)))	
  
  pval <- pf(f, ni.mean-1, (ni.mean-1)*(k-1), lower.tail=F)*k
  df1<- ni.mean-1
  df2<- (ni.mean-1)*(k-1)
  
  if (verbose) {
    cat("\n", "", METHOD, paste("(alpha = ", alpha, ")", 
                                sep = ""), "\n", sep = " ")
    cat("-------------------------------------------------------------", 
        "\n", sep = " ")
    cat("  data :", DNAME, "\n\n", sep = " ")
    cat("  statistic  :", G , "\n", sep = " ")
    cat("  num df     :", df1, "\n", sep = " ")
    cat("  denom df   :", df2, "\n", sep = " ")
    cat("  p.value    :", pval, "\n\n", sep = " ")
    cat(if (pval > alpha) {
      "  Result     : Variances are homogeneous."
    }
    else {
      "  Result     : Variances are not homogeneous."
    }, "\n")
    cat("-------------------------------------------------------------", 
        "\n\n", sep = " ")
  }
  
  result <- list()
  result$statistic <- G
  result$parameter <- c(df1, df2)
  result$p.value <- pval
  result$alpha <- alpha
  result$method <- METHOD
  result$data <- data
  result$formula <- formula
  invisible(result)
   
}
