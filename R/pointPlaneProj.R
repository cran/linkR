pointPlaneProj <- function(q, p, n){

	# PROJECTION OF POINT Q ONTO PLANE DEFINED BY POINT P AND NORMAL VECTOR N

	q - sum((q - p) * n) * n
}