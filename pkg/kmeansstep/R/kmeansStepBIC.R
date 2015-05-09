kmeansStepBIC <-
function(x, centers = 1, iter.max = 10, nstart = 10, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), trace=FALSE ){
oldBIC <- kmeansBIC(kmeans(x, centers, iter.max, nstart, algorithm, trace))
centers <- centers + 1
newBIC <- kmeansBIC(kmeans(x, centers, iter.max, nstart, algorithm, trace))
while(oldBIC > newBIC)
{
oldBIC <- newBIC
centers <- centers + 1
newBIC <- kmeansBIC(kmeans(x, centers, iter.max, nstart, algorithm, trace))
}
return(list(BIC = oldBIC, kmeans = kmeans(x, centers = centers - 1, iter.max, nstart, algorithm, trace)))
}
