avectors <- function(u, v){

	if(sqrt(sum(u * u)) == 0) stop("Input vector 'u' is zero-length")
	if(sqrt(sum(v * v)) == 0) stop("Input vector 'v' is zero-length")

	u <- uvector(u)
	v <- uvector(v)

	c <- sum(u*v) / sqrt(sum(u*u)) * sqrt(sum(v*v))
	if(round(c, digits=12) == 1) return(0)

	acos(c)
	#acos( sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )
}