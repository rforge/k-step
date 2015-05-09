kmeansStepEllbowAIC <-
function(x, centers = 1, iter.max = 10, nstart = 10, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), trace=FALSE ){
firstAIC <- kmeansAIC(kmeans(x, centers, iter.max, nstart, algorithm, trace))
centers <- centers + 1
secondAIC <- kmeansAIC(kmeans(x, centers, iter.max, nstart, algorithm, trace))
centers <- centers + 1
thirdAIC <- kmeansAIC(kmeans(x, centers, iter.max, nstart, algorithm, trace))
centers <- centers + 1
fourthAIC <- kmeansAIC(kmeans(x, centers, iter.max, nstart, algorithm, trace))
el_first <- (firstAIC - secondAIC)/(secondAIC - thirdAIC)
el_second <- (secondAIC - thirdAIC)/(thirdAIC - fourthAIC)

while(el_second > el_first)
{
firstAIC <- secondAIC
secondAIC <- thirdAIC
thirdAIC <- fourthAIC
centers <- centers + 1
fourthAIC <- kmeansAIC(kmeans(x, centers, iter.max, nstart, algorithm, trace))
el_first <- el_second
el_second <- (secondAIC - thirdAIC)/(thirdAIC - fourthAIC)
}
return(list(AIC = secondAIC, kmeans = kmeans(x, centers = centers - 2, iter.max, nstart, algorithm, trace)))
}
