bartletts.test <- function (formula, data,alpha = 0.05, na.rm = TRUE, verbose = TRUE) 
{
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  METHOD <- "Bartlett's Test"
  
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
  
  sp.nom<- list()
  
  for(i in x.levels) {
    sp.nom[[i]] <- (ni[i]-1)*y.variance[i]
  }
  
  sp.nom <- sum(unlist(sp.nom))
  sp.denom<- n-k
  sp<- sp.nom/sp.denom
  
  A<- (n-k)*log(sp)
  
  
  B <- list()
  for(i in x.levels) {
    B[[i]] <- (ni[i]-1)*log(y.variance[i])
  }
  B <- sum(unlist(B))
  
  
  C<- 1/(3*(k-1))
  
  
  D.1 <- list()
  for(i in x.levels) {
    D.1[[i]] <- 1/(ni[i]-1)
  }
  D.1<- sum(unlist(D.1)) 
  D<- D.1- 1/(n-k)
  
  Btest<- (A-B)/(1+(C*D))
  
  df = k-1
  
  p.value = pchisq(Btest, df, lower.tail = F)
  
  if (verbose) {
    cat("\n", "", METHOD, paste("(alpha = ", alpha, ")", 
                                sep = ""), "\n", sep = " ")
    cat("-------------------------------------------------------------", 
        "\n", sep = " ")
    cat("  data :", DNAME, "\n\n", sep = " ")
    cat("  statistic  :", Btest, "\n", sep = " ")
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
  result$statistic <- Btest
  result$parameter <- df
  result$p.value <- p.value
  result$alpha <- alpha
  result$method <- METHOD
  result$data <- data
  result$formula <- formula
  invisible(result)
}

