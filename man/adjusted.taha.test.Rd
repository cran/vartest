\name{adjusted.taha.test}
\alias{adjusted.taha.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Adjusted Taha Test
}
\description{
\code{adjusted.taha.test} performs Adjusted Taha variance homogeneity test.
}

\usage{adjusted.taha.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a tibble or data frame containing the variables in \code{formula}.} 
  \item{alpha}{the level of significance to assess variance homogeneity. Default is set to alpha = 0.05.}
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
  \item{verbose}{a logical for printing output to R console.}

}



\value{
A list containing the following components:
\item{statistic}{the corresponding test statistic.}
\item{parameter}{the parameter(s) of the approximate chi squared distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{method}{the character string "Adjusted Taha Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}

}



\references{

Conover, W.J., Iman, R.L.(1978). Some Exact Tables For The Squared Ranks Test. \emph{Communications in Statistics - Simulation and Computation}, \bold{B7(5)}, 491-513.

Fligner, M.A., Killeen, T.J.(1976). Distribution-Free Two-Sample Tests for Scale. \emph{Journal of the American Statistical Association}, \bold{71:353}, 210-213.

}



\author{
Gozde Cosar and Osman Dag
}




\examples{

library(vartest)

adjusted.taha.test(Sepal.Length ~ Species, data = iris)


}

\keyword{functions}
