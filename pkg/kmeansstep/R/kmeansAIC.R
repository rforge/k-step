kmeansAIC <-
function(fit){
m = ncol(fit$centers)
k = nrow(fit$centers)
D = fit$tot.withinss
return(D + 2*m*k)
}
