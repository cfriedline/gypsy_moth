#A corrected Tracy-Widom test for population structure
#Daniel Shriner
#July 14, 2011

#Let dat be the MxN matrix of genotypes for M SNPs and N individuals, with genotypes coded as 0, 1, or
#2 copies of the minor allele. The following R function performs a corrected Tracy-Widom test for a
#large lead eigenvalue.

library(RMTstat)
twtest <- function(dat) {
	#estimate posterior allele frequencies
	p <- vector("numeric",nrow(dat))
	for (i in 1:nrow(dat)) p[i] <- (1+sum(dat[i,],na.rm=TRUE))/(2+2*sum(!is.na(dat[i,])))

	#center
	mu <- apply(dat,1,mean,na.rm=TRUE)
	dat <- dat - mu
	dat[is.na(dat)] <- 0

	#normalize
	for (i in 1:nrow(dat)) dat[i,] <- dat[i,]/sqrt(p[i]*(1-p[i]))

	#perform eigendecomposition of the covariance matrix
	a <- eigen(cov(dat,use="complete.obs"))
	m <- length(a$values)

	#test each dimension
	res <- matrix(data=NA,nrow=(m-1),ncol=5)
	colnames(res) <- c("Dimension","Eigenvalue","nhat","TWstat","P-value")
	for (j in 1:(m-1)) {
		L1 <- sum(a$values[j:m])
		L2 <- sum(a$values[j:m]^2)
		lambda <- a$values[j]*(m-j)/L1
		nhat <- L1^2/L2
		mu <- (sqrt(nhat-1)+sqrt(m-1))^2/nhat
		sigma <- (sqrt(nhat-1)+sqrt(m-1))/nhat*(1/sqrt(nhat-1)+1/sqrt(m-1))^(1/3)
		twstat <- (lambda-mu)/sigma
		twpvalue <- ptw(twstat,lower.tail=FALSE)
		res[j,1] <- j
		res[j,2] <- lambda
		res[j,3] <- nhat
		res[j,4] <- twstat
		res[j,5] <- twpvalue
	}
	return(res)
}
