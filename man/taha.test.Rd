\name{taha.test}
\alias{taha.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Taha Test
}
\description{
\code{taha.test} performs Taha variance homogeneity test.
}

\usage{taha.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

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
\item{method}{the character string "Taha Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}

}



\references{

Conover, W.J., Iman, R.L.(1978). Some Exact Tables For The Squared Ranks Test. \emph{Communications in Statistics - Simulation and Computation}, \bold{B7(5)}, 491-513.

Taha, M.A.H. (1964). Rank Test for Scale Parameter for Asymmetrical One-Sided Distributions. \emph{Annales de l'ISUP}, \bold{13:3}, 169-180.

}



\author{
Gozde Cosar and Osman Dag
}

\seealso{
\code{\link[coin]{taha_test}}
}



\examples{

library(vartest)

taha.test(Sepal.Length ~ Species, data = iris)


}

\keyword{functions}
