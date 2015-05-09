\name{kmeansStepEllbowAIC}
\alias{kmeansStepEllbowAIC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
stepwise modelselection of k-means cluster using AIC and Ellbow-Method
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
stepwise modelselection of k-means cluster using AIC and Ellbow-Method
}
\usage{
kmeansStepEllbowAIC(x, centers = 1, iter.max = 10, nstart = 10, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), trace = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{centers}{
%%     ~~Describe \code{centers} here~~
}
  \item{iter.max}{
%%     ~~Describe \code{iter.max} here~~
}
  \item{nstart}{
%%     ~~Describe \code{nstart} here~~
}
  \item{algorithm}{
%%     ~~Describe \code{algorithm} here~~
}
  \item{trace}{
%%     ~~Describe \code{trace} here~~
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
function (x, centers = 1, iter.max = 10, nstart = 10, algorithm = c("Hartigan-Wong", 
    "Lloyd", "Forgy", "MacQueen"), trace = FALSE) 
{
    firstAIC <- kmeansAIC(kmeans(x, centers, iter.max, nstart, 
        algorithm, trace))
    centers <- centers + 1
    secondAIC <- kmeansAIC(kmeans(x, centers, iter.max, nstart, 
        algorithm, trace))
    centers <- centers + 1
    thirdAIC <- kmeansAIC(kmeans(x, centers, iter.max, nstart, 
        algorithm, trace))
    centers <- centers + 1
    fourthAIC <- kmeansAIC(kmeans(x, centers, iter.max, nstart, 
        algorithm, trace))
    el_first <- (firstAIC - secondAIC)/(secondAIC - thirdAIC)
    el_second <- (secondAIC - thirdAIC)/(thirdAIC - fourthAIC)
    while (el_second > el_first) {
        firstAIC <- secondAIC
        secondAIC <- thirdAIC
        thirdAIC <- fourthAIC
        centers <- centers + 1
        fourthAIC <- kmeansAIC(kmeans(x, centers, iter.max, nstart, 
            algorithm, trace))
        el_first <- el_second
        el_second <- (secondAIC - thirdAIC)/(thirdAIC - fourthAIC)
    }
    return(list(AIC = secondAIC, kmeans = kmeans(x, centers = centers - 
        2, iter.max, nstart, algorithm, trace)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
