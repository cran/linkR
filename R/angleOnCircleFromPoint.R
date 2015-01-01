angleOnCircleFromPoint <- function(circle, dist, P, point_compare=NULL, rerun=NULL){

	# GIVEN A CIRCLE, DISTANCE AND POINT P THIS FUNCTION FINDS THE ANGLE OF THE CIRCLE, SPECIFYING A
	#	POINT ON THE CIRCLE AT THE GIVEN DISTANCE TO POINT P. THERE WILL EITHER BE ZERO, TWO OR AN
	#	INFINITE NUMBER OF SOLUTIONS. FOR A TWO-POINT SOLUTION, IF point_compare IS GIVEN THEN THE
	#	POINT CLOSEST TO point_compare IS RETURNED.

	# SOLVE FOR ANGLE USING HARMONIC ADDITION THEOREM
	a <- 2*circle$R*(sum(circle$U*(circle$C-P)))
	b <- 2*circle$R*(sum(circle$V*(circle$C-P)))
	de <- atan(-b/a)
	
	# GET SIGN OF A
	if(a == 0){sign_a <- 1}else{sign_a <- sign(a)}
	
	# SOLVE FOR C
	c <- sign_a*sqrt(a^2 + b^2)

	f <- (dist^2 - sum(circle$C^2) + 2*sum(circle$C*P) - sum(P^2) - (circle$R^2)) / c
	if(abs(f) > 1) return(NA)
	r1 <- +acos(f) - de
	r2 <- -acos(f) - de

	d1 <- distPointToPoint(P, circlePoint(circle, r1))
	d2 <- distPointToPoint(P, circlePoint(circle, r2))

	if(distPointToPoint(d1, dist) > 10^-10 || distPointToPoint(d2, dist) > 10^-10)
		warnings("Warning: Solution does not conform to specified distance\n")
	
	if(!is.null(point_compare)){

		# FIND POINTS CORRESPONDING TO OUTPUT ANGLES
		output_points <- circlePoint(circle, c(r1, r2))
	
		# FIND POINT CLOSEST TO PREVIOUS POINT AND CORRESPONDING ANGLE
		dist_to_point_compare <- distPointToPoint(output_points, point_compare)
		if(identical(dist_to_point_compare[1], dist_to_point_compare[2])) return(asin(sin(r1)))
		return(c(r1, r2)[dist_to_point_compare == min(dist_to_point_compare)])
	}

	c(asin(sin(r1)), asin(sin(r2)))
}