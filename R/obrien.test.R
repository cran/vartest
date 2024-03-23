obrien.test<-function (formula, data, center = "mean", trim.rate = 0.25, alpha = 0.05, na.rm = TRUE, verbose = TRUE) 
{
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  METHOD <- "O'Brien Test"
  
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
  ni<- tapply(y, group, length)
  vars<- tapply(y, group, var)
  
    
  if(center=="mean"){y.means <- tapply(y, group, mean)
  }else if(center=="median"){y.means <- tapply(y, group, median)
  }else if(center=="trim.mean"){y.means <- tapply(y, group, mean, trim = trim.rate)
  }else {stop("Please correct center option.")
  }

  
  vij <- list()
  
  for(i in x.levels) {
    vij[[i]] <- ((ni[i]-1.5)*ni[i]*(y[group==i]-y.means[i])^2-0.5*vars[i]*(ni[i]-1))/((ni[i]-1)*(ni[i]-2))
  }
  
  v.means<- unlist(lapply(1:k, function(i) mean(vij[[i]])))

  v.mean <- sum(unlist(vij))/n
  v.n <- ni
  
  nom <- (n-k)*sum(v.n*((v.means-v.mean)^2))
  v_gstotal <- unlist(lapply(1:k, function(i) sum((vij[[i]]- mean(vij[[i]]))^2)))
  denom<- (k-1)*sum(v_gstotal)
  
  Obtest= nom/denom
  df1= k-1
  df2= n-k
  
  p.value = pf(Obtest, df1, df2, lower.tail = F)
  
  if (verbose) {
    cat("\n", "", METHOD, paste("(alpha = ", alpha, ")", 
                                sep = ""), "\n", sep = " ")
    cat("-------------------------------------------------------------", 
        "\n", sep = " ")
    cat("  data :", DNAME, "\n\n", sep = " ")
    cat("  statistic  :", Obtest, "\n", sep = " ")
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
  result$statistic <- Obtest
  result$parameter <- c(df1, df2)
  result$p.value <- p.value
  result$alpha <- alpha
  result$method <- METHOD
  result$data <- data
  result$formula <- formula
  invisible(result)
}
