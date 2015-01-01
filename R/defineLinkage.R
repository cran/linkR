defineLinkage <- function(joints=NULL, joints.type=NULL, joints.cvec=NULL, min.param=NULL, points=NULL, link.assoc=NULL, link.names=NULL, euler.vec=c(1,0,0)){

	# VALIDATE INPUTS
	if(!is.null(points) && is.null(link.assoc)) stop("'link.assoc' is NULL. If 'points' is defined, 'link.assoc' must be non-NULL.")

	# IF MIN PARAM IS LIST, CONVERT TO VECTOR
	if(!is.null(min.param) && is.list(min.param)) min.param <- unlist(min.param)

	# MAKE SURE JOINT TYPE LETTERS ARE UPPERCASE FOR STRING MATCHING
	if(!is.null(joints.type)) joints.type <- toupper(joints.type)

	# DEFAULT NULLS
	points.assoc <- NULL

	if(is.null(min.param)){

		# IF JOINT MATRIX IS 2D, ADD THIRD DIMENSION AS ZERO
		if(ncol(joints) == 2) joints <- cbind(joints, rep(0, nrow(joints)))

		if(!is.null(joints.cvec)){
			if(is.matrix(joints.cvec) && ncol(joints.cvec) == 2) joints.cvec <- cbind(joints.cvec, rep(0, nrow(joints.cvec)))
			if(is.list(joints.cvec)){
				for(i in 1:length(joints.cvec)) if(!is.na(joints.cvec[[i]][1]) && length(joints.cvec[[i]]) == 2) joints.cvec[[i]] <- c(joints.cvec[[i]], 0)
			}
		}

		# IF JOINT CONSTRAINT VECTORS ARE NULL, DEFINE AS PLANAR FOR 4-BAR
		if(nrow(joints) == 4 && is.null(joints.cvec)) joints.cvec <- matrix(c(0,0,1, NA,NA,NA, NA,NA,NA, 0,0,1), nrow=4, ncol=3, byrow=TRUE)

	}else{

		if(is.null(names(min.param))){
			stop("'min.param' must be a named vector or list. No names specified.")
			#names(min.param)[1:6] <- c('Li', 'Ria', 'Rib', 'Rig', 'L2', 'T1')
			#if(length(min.param) > 9) names(min.param)[7:13] <- c('E3a', 'E3b', 'E3g', 'Lo', 'Roa', 'Rob', 'Rog')
			#if(length(min.param) > 13) names(min.param)[14:17] <- c('L3', 'E4a', 'E4b', 'E4g')
			#if(length(min.param) == 9 && length(min.param) > 13) names(min.param)[(length(min.param)-2):length(min.param)] <- c('S3a', 'S3b', 'S3g')
		}
		
		if(length(min.param) == 4) min.param <- c(min.param, setNames(c(0, pi/2, 0, 0, 0), c('Ria', 'Rib', 'Rig', 'S3b', 'S3g')))
		if(length(min.param) == 5) min.param <- c(min.param, setNames(c(0, 0, 0, pi/2, 0, 0, pi/2, 0), c('E3b', 'E3g', 'Ria', 'Rib', 'Rig', 'Roa', 'Rob', 'Rog')))
		if(length(min.param) == 8) min.param <- c(min.param, setNames(c(0, 0, 0, pi/2, 0, 0, pi/2, 0, 0, 0, 0, 0), c('E3b', 'E3g', 'Ria', 'Rib', 'Rig', 'Roa', 'Rob', 'Rog', 'E4b', 'E4g', 'S3b', 'S3g')))

		# TRANSMISSION LINK AS VECTOR FROM JOINT 2 ROTATED INTO THE XY PLANE
		v12 <- rotationMatrixZYX(c(min.param['T1'],0,0)) %*% c(0, min.param['L2'], 0)

		# DEFINE FIRST THREE JOINTS
		joints <- matrix(c(0,0,0, 0,min.param['Li'],0, v12[1],min.param['Li']+v12[2],0), nrow=3, ncol=3, byrow=TRUE)

		if(length(min.param) == 13){

			# ADD FOURTH JOINT
			joints <- rbind(joints, joints[3, ] + (uvector(joints[3, ] - joints[2, ]) * min.param['Lo']) %*% rotationMatrixZYX(min.param[c('E3a', 'E3b', 'E3g')]))
		}
		if(length(min.param) == 20){

			# ADD FOURTH AND FIFTH JOINTS
			joints <- rbind(joints, joints[3, ] + (uvector(joints[3, ] - joints[2, ]) * min.param['L3']) %*% rotationMatrixZYX(min.param[c('E3a', 'E3b', 'E3g')]))
			joints <- rbind(joints, joints[4, ] + (uvector(joints[4, ] - joints[3, ]) * min.param['Lo']) %*% rotationMatrixZYX(min.param[c('E4a', 'E4b', 'E4g')]))
		}

		# FIND AXES OF ROTATION USING EULER ANGLES
		joints.cvec <- matrix(NA, nrow=nrow(joints), ncol=3)
		joints.cvec[1, ] <- euler.vec %*% rotationMatrixZYX(min.param[c('Ria', 'Rib', 'Rig')])
		if(length(min.param) > 9) joints.cvec[nrow(joints.cvec), ] <- euler.vec %*% rotationMatrixZYX(min.param[c('Roa', 'Rob', 'Rog')])
		
		# ADD SLIDE VECTOR
		if('S3a' %in% names(min.param)) joints.cvec[3, ] <- uvector(euler.vec %*% rotationMatrixZYX(min.param[c('S3a', 'S3b', 'S3g')]))

		# SPECIFY JOINT TYPES
		if(nrow(joints) == 3) joints.type <- c("R", "U", "L")
		if(nrow(joints) == 4) joints.type <- c("R", "U", "U", "R")
		if(nrow(joints) == 5) joints.type <- c("R", "U", "L", "U", "R")
	}

	# DEFINE NUMBER OF DEGREES OF FREEDOM
	dof_list <- list(
		c('LL', 'LLL', 'RU', 'LUR', 'LLUR', 'RUL', 'RUUR', 'RULUR'),
		c('LURR', 'LPUR')
		)

	# MAKE SURE THAT LINKAGE TYPE IS ALLOWED
	supported_types <- unlist(dof_list)
	if(!paste(joints.type, collapse='') %in% supported_types) 
		stop(paste0("The input linkage of type '", paste(joints.type, collapse=''), "' is not currently supported. Currently supported linkage types are: ", paste(supported_types, collapse=", "), "."))

	#if(!is.null(link.assoc)){
	#	if(is.null(link.names) && length(link.assoc) != nrow(joints)) stop(paste0("If 'link.names' is NULL, the length of 'link.assoc' should correspond to the number of links (", nrow(joints), ")."))
	#}

	# IF JOINT CONSTRAINTS ARE LIST, CONVERT TO MATRIX
	if(is.list(joints.cvec)){
		joints_cvec <- matrix(NA, nrow=nrow(joints), ncol=3)
		for(i in 1:nrow(joints)) if(!is.na(joints.cvec[[i]][1])) joints_cvec[i, ] <- unlist(joints.cvec[[i]])
		joints.cvec <- joints_cvec
	}

	# ASSIGN DEGREES OF FREEDOM
	for(i in 1:length(dof_list)) if(paste(joints.type, collapse='') %in% dof_list[[i]]) dof <- i

	# ADD ROWNAMES TO JOINTS
	rownames(joints) <- paste0("Joint", 1:nrow(joints))

	# ADD ROWNAMES TO CONSTRAINT VECTOR LIST
	rownames(joints.cvec) <- paste0("Joint", 1:nrow(joints.cvec))
	
	# MAKE CONSTRAINT VECTORS UNIT VECTORS
	for(i in 1:nrow(joints.cvec)) if(!is.na(joints.cvec[i, 1])) joints.cvec[i, ] <- uvector(joints.cvec[i, ])

	if(!is.null(points)){
		
		# IF POINTS ARE VECTOR CONVERT TO MATRIX
		if(is.vector(points)) points <- matrix(points, ncol=length(points))

		# IF POINT MATRIX IS 2D, ADD THIRD DIMENSION AS ZERO
		if(ncol(points) == 2) points <- cbind(points, rep(0, nrow(points)))
		
		# MAKE SURE LINK.ASSOC IS OF THE SAME LENGTH AS POINTS
		if(length(link.assoc) != nrow(points)) stop(paste0("The length of 'link.assoc' (", length(link.assoc), ") must be the same as the number of rows in 'points' (", nrow(points), ")."))

		# IF LINK.NAMES IS NULL, SET TO DEFAULT
		if(is.null(link.names)) link.names <- paste0("Link", 1:nrow(joints))

		# SET THE POINTS ASSOCIATED WITH EACH LINK
		points.assoc <- setNames(vector("list", nrow(joints)), link.names)			
		
		# IF LINK.ASSOC ARE NUMERIC INTEGERS
		if(is.numeric(link.assoc[1])){
			for(i in 1:length(link.assoc))
				points.assoc[[names(points.assoc)[link.assoc[i]]]] <- c(points.assoc[[names(points.assoc)[link.assoc[i]]]], i)
		}else{
			for(i in 1:length(link.assoc)) points.assoc[[link.assoc[i]]] <- c(points.assoc[[link.assoc[i]]], i)
		}
	}

	min_param_define <- FALSE
	if(nrow(joints) == 3 && sum(joints.type == c('R', 'U', 'L')) == 3) min_param_define <- TRUE
	if(nrow(joints) == 4 && sum(joints.type == c('R', 'U', 'U', 'R')) == 4) min_param_define <- TRUE
	if(nrow(joints) == 5 && sum(joints.type == c('R', 'U', 'L', 'U', 'R')) == 5) min_param_define <- TRUE
	
	if(min_param_define){

		# MINIMUM PARAMETER VECTOR
		if(nrow(joints) == 3) min.param <- rep(NA, 9)
		if(nrow(joints) == 4) min.param <- rep(NA, 13)
		if(nrow(joints) == 5) min.param <- rep(NA, 20)

		# SET VECTOR NAMES
		names(min.param)[1:6] <- c('Li', 'Ria', 'Rib', 'Rig', 'L2', 'T1')
		if(nrow(joints) >= 4) names(min.param)[7:13] <- c('E3a', 'E3b', 'E3g', 'Lo', 'Roa', 'Rob', 'Rog')
		if(nrow(joints) >= 5) names(min.param)[14:17] <- c('L3', 'E4a', 'E4b', 'E4g')
		if("L" %in% joints.type || "P" %in% joints.type) names(min.param)[(length(min.param)-2):length(min.param)] <- c('S3a', 'S3b', 'S3g')

		# LENGTH OF LINKS
		min.param['Li'] <- distPointToPoint(joints[c(1, 2), ])
		min.param['L2'] <- distPointToPoint(joints[c(2, 3), ])

		# ANGLE BETWEEN INPUT AND TRANSMISSION LINK
		min.param['T1'] <- avectors(joints[1, ] - joints[2, ], joints[3, ] - joints[2, ])

		# TRANSMISSION LINK AS VECTOR FROM JOINT 2 ROTATED INTO THE XY PLANE
		v12 <- rotationMatrixZYX(c(min.param['T1'],0,0)) %*% c(0, distPointToPoint(joints[c(2, 3), ]), 0)

		# A SECOND MATRIX OF THE FIRST THREE JOINTS, ORIGIN AT 0,0,0, JOINT1 AT 0,X,0 AND COPLANAR WITH THE XY PLANE
		m2 <- matrix(c(0,0,0, 0,min.param['Li'],0, v12[1],min.param['Li']+v12[2],0), nrow=3, ncol=3, byrow=TRUE)

		# IF THE ANGLE WAS OFF BY 180 DEGREES, SUBTRACT 180 DEGREES TO GET PROPER COORDINATES
		if(distPointToPoint(m2[c(1,3),]) != distPointToPoint(joints[c(1,3),])){
			min.param['T1'] <- avectors(joints[1, ] - joints[2, ], joints[3, ] - joints[2, ]) - pi
			v12 <- rotationMatrixZYX(c(min.param['T1'],0,0)) %*% c(0, distPointToPoint(joints[c(2, 3), ]), 0)
			m2 <- matrix(c(0,0,0, 0,min.param['Li'],0, v12[1],min.param['Li']+v12[2],0), nrow=3, ncol=3, byrow=TRUE)
		}

		# TRANSFORM ALL JOINTS SO THAT THE FIRST THREE ARE IN THE XY PLANE
		joints_r <- copyTransformation(joints[1:3, ], m2, joints)

		# TRANSFORM ALL AXES OF ROTATION SO THAT THEY CORRESPOND WITH ORIGINAL ORIENTATIONS
		joints_cvec_r <- copyTransformation(joints[1:3, ], m2, joints.cvec, translate=FALSE)

		# AXES OF ROTATION FOR INPUT LINK AS EULER ANGLES
		min.param[c('Ria', 'Rib', 'Rig')] <- vectorsToEP(euler.vec, joints_cvec_r[1, ], 'left')[[1]]

		if(nrow(joints) >= 4){

			# LENGTH OF OUTPUT LINK
			min.param['Lo'] <- distPointToPoint(joints[c(nrow(joints)-1, nrow(joints)), ])

			# AXES OF ROTATION FOR OUTPUT LINK AS EULER ANGLES
			min.param[c('Roa', 'Rob', 'Rog')] <- vectorsToEP(euler.vec, joints_cvec_r[nrow(joints_cvec_r), ], 'left')[[1]]

			# ANGLE BETWEEN TRANSMISSION LINK AND OUTPUT LINK AS EULER ANGLES
			min.param[c('E3a', 'E3b', 'E3g')] <- vectorsToEP(joints_r[3, ]-joints_r[2,], joints_r[4, ]-joints_r[3,], 'left')[[1]]
		}

		if(nrow(joints) >= 5){

			# LENGTH OF LINK
			min.param['L3'] <- distPointToPoint(joints[c(3, 4), ])

			# ANGLE BETWEEN TRANSMISSION LINK AND OUTPUT LINK AS EULER ANGLES
			min.param[c('E4a', 'E4b', 'E4g')] <- vectorsToEP(joints_r[4, ]-joints_r[3,], joints_r[5, ]-joints_r[4,], 'left')[[1]]
		}

		if("L" %in% joints.type || "P" %in% joints.type){

			# TRANSFORM SLIDE SO THAT IT CORRESPONDS WITH ORIGINAL ORIENTATION
			slide_r <- copyTransformation(joints[1:3, ], m2, joints.cvec[3, ], translate=FALSE)

			# ANGLE BETWEEN TRANSMISSION LINK AND OUTPUT LINK AS EULER ANGLES
			min.param[c('S3a', 'S3b', 'S3g')] <- vectorsToEP(euler.vec, slide_r, 'left')[[1]]
		}

		# TEST THAT TRANSFORMED JOINTS HAVE THE SAME GEOMETRY AS ORIGINAL JOINTS
		joints_test <- copyTransformation(joints_r[1:3, ], joints[1:3, ], joints_r)
		#print(round(sum(joints - joints_test), 10))
		#print((euler.vec * min.param['L3']) %*% rotationMatrixZYX(ep))
	}else{
		min.param <- NULL
	}

	linkage <- list(
		'joints' = joints,
		'joints.cvec' = joints.cvec,
		'joints.type' = joints.type,
		'points' = points,
		'points.assoc' = points.assoc,
		'link.assoc' = link.assoc,
		'link.names' = link.names,
		'min.param' = min.param,
		'dof' = dof
	)

	class(linkage) <- 'linkage'

	linkage
}
