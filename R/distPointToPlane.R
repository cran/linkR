distPointToPlane <- function(p, n, q){
	# q is point in the plane

	n_u <- uvector(n)
	v <- q - p
	sum(abs(n_u*v))
}