kmeansStepEllbowBIC <-
function(x, centers = 1, iter.max = 10, nstart = 10, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), trace=FALSE ){
firstBIC <- kmeansBIC(kmeans(x, centers, iter.max, nstart, algorithm, trace))
centers <- centers + 1
secondBIC <- kmeansBIC(kmeans(x, centers, iter.max, nstart, algorithm, trace))
centers <- centers + 1
thirdBIC <- kmeansBIC(kmeans(x, centers, iter.max, nstart, algorithm, trace))
centers <- centers + 1
fourthBIC <- kmeansBIC(kmeans(x, centers, iter.max, nstart, algorithm, trace))
el_first <- (firstBIC - secondBIC)/(secondBIC - thirdBIC)
el_second <- (secondBIC - thirdBIC)/(thirdBIC - fourthBIC)

while(el_second > el_first)
{
firstBIC <- secondBIC
secondBIC <- thirdBIC
thirdBIC <- fourthBIC
centers <- centers + 1
fourthBIC <- kmeansBIC(kmeans(x, centers, iter.max, nstart, algorithm, trace))
el_first <- el_second
el_second <- (secondBIC - thirdBIC)/(thirdBIC - fourthBIC)
}
return(list(BIC = secondBIC, kmeans = kmeans(x, centers = centers - 2, iter.max, nstart, algorithm, trace)))
}
