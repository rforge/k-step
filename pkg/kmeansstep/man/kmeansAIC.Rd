\name{kmeansAIC}
\alias{kmeansAIC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
AIC of k-means cluster
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
calculate the AIC of a specific k-means cluster and it specified centroids
}
\usage{
kmeansAIC(fit)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{
%%     ~~Describe \code{fit} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
http://sherrytowers.com/2013/10/24/k-means-clustering/
}
\author{
%%  ~~who you are~~
Markus Mayer
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (fit) 
{
    m = ncol(fit$centers)
    k = nrow(fit$centers)
    D = fit$tot.withinss
    return(D + 2 * m * k)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
