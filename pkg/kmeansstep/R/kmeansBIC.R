kmeansBIC <-
function(fit){
m = ncol(fit$centers)
n = length(fit$cluster)
k = nrow(fit$centers)
D = fit$tot.withinss
return(D + log(n)*m*k)
}
