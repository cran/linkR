copyTransformation <- function(m1, m2, mn, translate=TRUE){

	if(nrow(m1) == 1) stop("'m1' must have a minimum of two rows.")
	if(nrow(m1) != nrow(m2)) stop(paste0("'m1' ", nrow(m1), " must have the same number of rows as 'm2' ", nrow(m2), "."))

	# IF AT LEAST TWO POINTS IN M1 ARE THE SAME, ONLY APPLY TRANSLATION
	if(sum(distPointToPoint(m1, m1[c(2:nrow(m1),1), ]) == 0) > 0)
		return(mn + matrix(m2[1, ] - m1[1, ], nrow=nrow(mn), ncol=ncol(mn), byrow=TRUE))

	# CONVERT TO MATRIX IF VECTOR
	if(is.vector(mn)) mn <- matrix(mn, 1, 3)

	# EMPTY RETURN MATRIX
	if(nrow(mn) == 0) return(mn)

	# TRANSFORMED POINT MATRIX
	mr <- matrix(NA, nrow=nrow(mn), ncol=ncol(mn))

	# TRANSFORMATION GIVEN ONLY TWO POINTS (MINIMIZE ROTATION ABOUT LONG AXIS)
	two_ref_pts <- ifelse(nrow(m1) == 2, TRUE, FALSE)

	if(two_ref_pts){
		m1 <- rbind(m1, m1[1, ] + uvector(vorthogonal(m1[2, ] - m1[1, ])))
		m2 <- rbind(m2, m2[1, ] + uvector(vorthogonal(m2[2, ] - m2[1, ])))
	}

	# BUG FIX: ADD IN CODE SO THAT IF TRANSFORMATION CAN BE SIMPLE TRANSLATION, DO NOT DO ANY ROTATION

	# INCLUDE ORIGIN
	if(!translate) mn <- rbind(mn, c(0,0,0))

	# FIND INITIAL COORDINATE SYSTEM AXES
	rm_i <- setCoordinateAxes(m1)

	# FIND INITIAL POSITION VECTOR MATRIX
	mn_ref <- (mn - matrix(m1[1, ], nrow=nrow(mn), ncol=3, byrow=TRUE)) %*% t(rm_i)

	# FIND FINAL COORDINATE SYSTEM AXES
	ca <- setCoordinateAxes(m2)

	# FIND ROTATION MATRIX TO TRANSFORM TO FINAL COORDINATE SYSTEM
	rm <- tMatrixDC(ca)
	
	# TRANSFORM POINTS
	mr <- (mn_ref %*% t(rm)) + matrix(m2[1, ], nrow=nrow(mn_ref), ncol=3, byrow=TRUE)

	# CENTER BY PREVIOUS ORIGIN
	if(!translate){
		mr <- mr - matrix(mr[nrow(mr), ], nrow=nrow(mr), ncol=3, byrow=TRUE)
		mr <- mr[1:(nrow(mr)-1), ]
	}

	if(two_ref_pts){
		
		# FIND A POINT NOT COINCIDENT WITH END POINTS
		ref_idx <- NA
		for(i in 1:nrow(mn)) if(distPointToLine(mn[i, ], m1[1, ], m1[2, ]) > 1e-13){ref_idx <- i;break}
			
		# IF ALL POINTS ARE COINCIDENT WITH END POINTS RETURN TRANSFORMED POINTS
		if(is.na(ref_idx)) return(mr)

		# FIND POINT ON LINE BETWEEN END POINTS, AT NORMAL TO REF POINT
		ref_norm <- pointNormalOnLine(mn[ref_idx, ], m1[1, ], m1[2, ])
		#print(distPointToLine(ref_norm, m1[1, ], m1[2, ]))
		
		# GET DISTANCE FROM LINE TO REF POINT
		ref_norm_dist <- distPointToPoint(mn[ref_idx, ], ref_norm)

		# TRANSFORM REF NORMAL USING SAME TRANSFORMATION AS TO MN
		ref_norm_t <- (((ref_norm - m1[1, ]) %*% t(rm_i)) %*% t(rm)) + m2[1, ]
		
		# VECTOR FROM REF NORM TO REF POINT
		ref_vector <- uvector(mn[ref_idx, ] - ref_norm)*ref_norm_dist

		# TRANSLATED REF POINT FROM REF NORM
		ref_toproj <- ref_proj <- ref_norm_t + ref_vector
		
		# FIND MINIMUM DISTANCE BETWEEN POINT AND TRANSFORMED POINT POTENTIAL LOCATIONS
		minDist <- distPointToPlane(p=ref_toproj, n=uvector(m2[2, ] - m2[1, ]), q=ref_norm_t)
		
		# PROJECT TRANSLATED REF POINT INTO PLANE
		if(minDist > 1e-10){

			ref_proj <- pointPlaneProj(q=ref_toproj, p=ref_norm_t, n=uvector(m2[2, ] - m2[1, ]))			
			#ref_proj <- ref_norm_t + uvector(ref_proj - ref_norm_t)*ref_norm_dist
		}

		# FIND ANGLE BETWEEN TRANSFORMED POINT AND PROJECTED POINT
		a <- avectors(mr[ref_idx, ] - ref_norm_t, ref_proj - ref_norm_t)
		
		# IF DIFFERENCE IN ANGLE IS 0, RETURN TRANSFORMED MATRIX AS IS
		if(a == 0) return(mr)

		# CENTER POINTS ABOUT ONE OF POINTS ON AXIS OF ROTATION
		mr_centroid_m <- matrix(m2[2, ], nrow=nrow(mr), ncol=3, byrow=TRUE)
		mrc <- mr - mr_centroid_m

		# APPLY ROTATIONS IN BOTH DIRECTIONS
		mrc_p <- mrc %*% tMatrixEP(m2[2, ]-m2[1, ], a=a)
		mrc_n <- mrc %*% tMatrixEP(m2[2, ]-m2[1, ], a=-a)

		# UNDO CENTERING TRANSLATION
		mrc_p <- mrc_p + mr_centroid_m
		mrc_n <- mrc_n + mr_centroid_m

		# IDENTIFY CORRECT DIRECTION OF ROTATION AND RETURN CORRESPONDING MATRIX
		ifelse(distPointToPoint(ref_proj, mrc_p[ref_idx, ]) < distPointToPoint(ref_proj, mrc_n[ref_idx, ]), return(mrc_p), return(mrc_n))
	}

	mr
}