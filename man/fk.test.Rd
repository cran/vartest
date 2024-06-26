\name{fk.test}
\alias{fk.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fligner-Killeen Test
}
\description{
\code{fk.test} performs Fligner-Killeen variance homogeneity test.
}

\usage{fk.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

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
\item{method}{the character string "Fligner-Killeen Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}

}



\references{

Fligner, M.A., Killeen, T.J.(1976). Distribution-Free Two-Sample Tests for Scale. \emph{Journal of the American Statistical Association}, \bold{71:353}, 210-213.

Conover, W.J., Johnson, M.E., Johnson, M.M. (1981). A Comparative Study of Tests for Homogeneity of Variances, with Applications to the Outer Continental Shelf Bidding Data. \emph{Technometrics}, \bold{23:4}, 351-361.

}



\author{
Gozde Cosar and Osman Dag
}

\seealso{
\code{\link[coin]{fligner_test}}
}



\examples{

library(vartest)

fk.test(Sepal.Length ~ Species, data = iris)


}

\keyword{functions}
