analyzeLinkage <- function(linkage, input=1){

	kt.by.animate <- TRUE
	kt.by.geometry <- TRUE
	xyz <- c("x", "y", "z")
	j_force <- NA
	moment.arm.o <- NA

	if(length(dim(linkage$joints)) == 3){

		alink_i <- list()

		# CALL ANALYZE LINKAGE FOR EACH ITERATION IN ARRAY
		for(i in 1:dim(linkage$joints)[3]){

			# REPLACE ARRAY OBJECTS WITH SINGLE ITERATION
			linkage_i <- linkage
			linkage_i$joints <- linkage$joints[, , i]
			if(!is.null(linkage$points)) linkage_i$points <- linkage$points[, , i]

			# CALL ANALYZE LINKAGE
			alink_i[[i]] <- analyzeLinkage(linkage_i, input)
		}

		# COPY OVER RESULTS FROM EACH ITERATION AS VECTOR
		analyze_linkage <- list()
		list_objects <- names(alink_i[[1]])
		for(list_object in list_objects){
			for(i in 1:length(alink_i)) analyze_linkage[[list_object]] <- c(analyze_linkage[[list_object]], alink_i[[i]][[list_object]])
		}

		class(analyze_linkage) <- 'analyze_linkage'

		return(analyze_linkage)
	}

	# DEFINE IMPORTANT INDICES
	if(input == 1){p_idx <- 1:nrow(linkage$joints)}else{p_idx <- nrow(linkage$joints):1}

	## FIND KT BY ANIMATING LINKAGE
	if(kt.by.animate){

		# ANIMATE LINKAGE OVER SMALL ANGLE
		linkage_animate <- animateLinkage(linkage, t=c(0, 0.0001))

		# GET DISPLACEMENTS OF EACH JOINT
		j_disp <- sqrt(rowSums((linkage_animate$joints[, , 2] - linkage_animate$joints[, , 1])^2))

		# GET CHANGE IN ANGLE OF INPUT MOMENT ARM
		input_ptrot <- pointNormalOnLine(linkage_animate$joints[p_idx[2], , 1], linkage_animate$joints[p_idx[1], , 1], linkage_animate$joints[p_idx[1], , 1]+linkage$joints.cvec[p_idx[1],])
		input_vector1 <- linkage_animate$joints[p_idx[2], , 1] - input_ptrot
		input_vector2 <- linkage_animate$joints[p_idx[2], , 2] - input_ptrot
		
		# GET INPUT MOMENT ARM LENGTH
		moment.arm.i <- input_vector1

		if(nrow(linkage$joints) == 3){

			# TAKE RATIO OF DISPLACEMENT OF 3RD JOINT OVER INPUT ANGLE CHANGE
			kt.animate <- j_disp[3] / avectors(input_vector1, input_vector2)

		}else{

			# GET CHANGE IN ANGLE OF OUTPUT MOMENT ARM
			output_ptrot <- pointNormalOnLine(linkage_animate$joints[p_idx[length(p_idx)-1], , 1], linkage_animate$joints[tail(p_idx, 1), , 1], linkage_animate$joints[tail(p_idx, 1), , 1]+linkage$joints.cvec[tail(p_idx, 1),])
			output_vector1 <- linkage_animate$joints[p_idx[length(p_idx)-1], , 1] - output_ptrot
			output_vector2 <- linkage_animate$joints[p_idx[length(p_idx)-1], , 2] - output_ptrot
		
			# TAKE RATIO OF ANGLE CHANGE
			#cat("O", avectors(output_vector1, output_vector2), "\n")
			#cat("I", avectors(input_vector1, input_vector2), "\n")			
			kt.animate <- avectors(output_vector1, output_vector2) / avectors(input_vector1, input_vector2)
			
			# GET OUTPUT MOMENT ARM LENGTH
			moment.arm.o <- output_vector1
		}
	}else{
		kt.animate <- NA
	}

	## FIND KT BY GEOMETRY
	if(kt.by.geometry){

		# DEFINE INPUT VECTOR
		input_vector <- linkage$joints[p_idx[1], ] - linkage$joints[p_idx[2], ]

		# TRANSMISSION LINK VECTOR
		tlink_vector1 <- tlink_vector2 <- linkage$joints[p_idx[3], ] - linkage$joints[p_idx[2], ]

		# ANGLE BETWEEN INPUT LINK AND TRANSMISSION LINK
		alpha_i <- avectors(input_vector, tlink_vector1)

		# CROSS PRODUCT VECTORS OF TRANSMISSION LINK AND INPUT LINK
		tlink_in_cprod <- uvector(cprod(tlink_vector1, input_vector))

		# BETA FOR INPUT LINK
		beta_i <- avectors(tlink_in_cprod, linkage$joints.cvec[p_idx[1], ])

		# FOR 4-BAR, SET SLIDE TO BE PARALLEL TO TRANSMISSION LINK
		if(nrow(linkage$joints) == 4) linkage$joints.cvec[3, ] <- tlink_vector1

		# ANGLE BETWEEN TRANSMISSION LINK AND SLIDE VECTOR
		gamma_i <- avectors(tlink_vector1, linkage$joints.cvec[3, ])

		if(nrow(linkage$joints) == 3){

			linkage$min.param['Lo'] <- 1
			alpha_o <- pi/2
			beta_o <- gamma_o <- 0

		}else{

			# DEFINE OUTPUT VECTOR
			output_vector <- linkage$joints[tail(p_idx, 1), ] - linkage$joints[p_idx[length(p_idx)-1], ]
	
			# TRANSMISSION LINK VECTOR
			if(nrow(linkage$joints) == 5) tlink_vector2 <- linkage$joints[p_idx[3], ] - linkage$joints[p_idx[4], ]

			# ANGLE BETWEEN OUTPUT LINK AND TRANSMISSION LINK
			alpha_o <- avectors(output_vector, tlink_vector2)

			# CROSS PRODUCT VECTORS OF TRANSMISSION LINK AND OUTPUT LINK
			tlink_out_cprod <- uvector(cprod(tlink_vector2, output_vector))

			# BETA FOR OUTPUT LINK
			beta_o <- avectors(tlink_out_cprod, linkage$joints.cvec[tail(p_idx, 1), ])

			# ANGLE BETWEEN TRANSMISSION LINK AND SLIDE VECTOR
			#print(uvector(tlink_vector2))
			#print(uvector(linkage$joints.cvec[3, ]))
			#print(linkage$joints.cvec)
			gamma_o <- avectors(tlink_vector2, linkage$joints.cvec[3, ])
			#print(gamma_o)
			#cat('\n')
		}

		# MAKE SURE ANGLES ARE MINIMUM
		#alpha_i <- ifelse(abs(alpha_i) > pi/2, abs(alpha_i) - pi/2, abs(alpha_i))
		#alpha_o <- ifelse(abs(alpha_o) > pi/2, abs(alpha_o) - pi/2, abs(alpha_o))
		#beta_i <- ifelse(abs(beta_i) > pi/2, pi - abs(beta_i), abs(beta_i))
		#beta_o <- ifelse(abs(beta_o) > pi/2, pi - abs(beta_o), abs(beta_o))
		gamma_i <- abs(c(gamma_i-pi, gamma_i)[which.min(abs(c(gamma_i-pi, gamma_i)))])
		gamma_o <- abs(c(gamma_o-pi, gamma_o)[which.min(abs(c(gamma_o-pi, gamma_o)))])
		
		# RATIOS
		inout_ratio <- linkage$min.param['Li'] / linkage$min.param['Lo']
		alpha_ratio <- sin(alpha_i)/sin(alpha_o)
		beta_ratio <- cos(beta_i)/cos(beta_o)
		gamma_ratio <- cos(gamma_o)/cos(gamma_i)

		# CALCULATE KT
		kt.geometry <- abs(inout_ratio * alpha_ratio * beta_ratio * gamma_ratio)

		#kt.geometry <- linkage$min.param['Li'] * sin(alpha_i) * cos(beta_i) * (1/cos(gamma_i))
	}else{
		kt.geometry <- NA
	}

	# PRINT KT FROM EACH METHOD
	#cat(round(sd(abs(c(kt.animate, kt.geometry))), 4), "\n")
	#cat(kt.animate, ", ", kt.geometry, "\n", sep="")

	analyze_linkage <- list(
		'kt' = setNames(kt.geometry, ""),
		'ma' = setNames(1/kt.geometry, ""),
		'alpha.i' = alpha_i,
		'alpha.o' = alpha_o,
		'alpha.ratio' = abs(alpha_ratio),
		'beta.i' = beta_i,
		'beta.o' = beta_o,
		'beta.ratio' = abs(beta_ratio),
		'gamma.i' = gamma_i,
		'gamma.o' = gamma_o,
		'gamma.ratio' = abs(gamma_ratio),
		'inout.ratio' = setNames(abs(inout_ratio), ""),
		'kt.animate' = setNames(kt.animate, ""),
		'kt.geometry' = setNames(kt.geometry, ""),
		'j.disp' = j_disp, 'j.force' = j_force,
		'moment.arm.i' = moment.arm.i, 'moment.arm.o' = moment.arm.o
	)

	class(analyze_linkage) <- 'analyze_linkage'

	return(analyze_linkage)
}
