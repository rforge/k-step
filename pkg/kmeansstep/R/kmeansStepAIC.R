kmeansStepAIC <-
function(x, centers = 1, iter.max = 10, nstart = 10, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), trace=FALSE ){
oldAIC <- kmeansAIC(kmeans(x, centers, iter.max, nstart, algorithm, trace))
centers <- centers + 1
newAIC <- kmeansAIC(kmeans(x, centers, iter.max, nstart, algorithm, trace))
while(oldAIC > newAIC)
{
oldAIC <- newAIC
centers <- centers + 1
newAIC <- kmeansAIC(kmeans(x, centers, iter.max, nstart, algorithm, trace))
}
return(list(AIC = oldAIC, kmeans = kmeans(x, centers = centers - 1, iter.max, nstart, algorithm, trace)))
}
