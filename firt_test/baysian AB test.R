test1 <- as.table(rbind(c(2360, 2360-34), c(2361,2361-31)))
dimnames(test1) <- list(group = c("A", "D"), type=c("S", "F"))


test2 <- as.table(rbind(c(1960,1960-68), c(2084,2084-72)))
dimnames(test2) <- list(group = c("B","C"), type=c("S","F"))



prob.a.better.d <- integrate(function(x){
					  dbeta(x, 34+1, 2360-34+1)*pbeta(x,31+1,2361-31+1)
				     },0,1)

prob.d.better.a <- integrate(function(x) {
					  dbeta(x, 31+1, 2361-31+1)*pbeta(x, 34+1, 2360-34+1)
				     }, 0, 1)

prob.b.better.c <- integrate(function(x){
					  dbeta(x, 68+1, 1960-68+1)*pbeta(x,72+1,2084-72+1)
				     },0,1)

prob.c.better.b <- integrate(function(x) {
					  dbeta(x, 72+1, 2084-72+1)*pbeta(x, 68+1, 1960-68+1)
				     }, 0, 1)

bab <- function(data) {
	k <- nrow(data)
	result <- numeric(k)
	for(i in(1:k)){
      idx <- (1:k)[-i]
	f <- function(z) {
	   r <- dbeta(z,
			  data[i,'conversion']+1,
			  data[i,'trial'] - data[i,'conversion']+1)
	   for (j in idx) {
	     r <- r*pbeta(z,
				data[j,'conversion']+1,
				data[j,'trial'] - data[j,'conversion'] +1)
				}
				return(r)
      }
	result[i] = integrate(f, 0, 1)$value
 }
 return(result)
}


