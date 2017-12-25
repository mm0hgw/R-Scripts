# Created by https://github.com/mm0hgw

swapHalves <- function(x){
	lx <- length(x)
	key1 <- seq(lx%/%2)
	key2 <- seq(to=lx,length.out=length(key1))
	out <- x
	out[key1] <- x[key2]
	out[key2] <- x[key1]
	out	
}

