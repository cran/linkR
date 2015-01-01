animateLinkage <- function(linkage, t, input.link=NULL){

	if(class(linkage) == 'linkage_system'){
	
		# CREATE ANIMATED LINKAGE SYSTEM
		linkage_r <- list()
		class(linkage_r) <- 'linkage_system'
		
		# COPY OVER SYSTEM LEVEL STRUCTURES
		points <- linkage$points
		link.names <- linkage$link.names
		link.assoc <- linkage$link.assoc
		points.assoc <- linkage$points.assoc
		
		# GET NUMBER OF LINKAGES
		linkage_ct <- sum(names(linkage) == "")
		
		# CHECK THAT INPUT LINK IS SPECIFIED
		if(is.null(input.link)) stop("For input of linkage system into animateLinkage, input.link must be defined.")

		# CREATE INPUT PARAMETER LIST
		input_parameters <- list()
		
		# INITIAL INPUT PARAMETERS
		input_parameters[[input.link]] <- t
		
		# RECORD IF LINKAGE HAS BEEN ANIMATED
		animated <- rep(FALSE, linkage_ct)

		n <- 0
		while(n < 5){

			for(i in 1:linkage_ct){

				# CHECK IF LINKAGE HAS ALREADY BEEN ANIMATED
				if(animated[i]) next

				# CHECK THAT NUMBER OF AVAILABLE INPUT PARAMETERS MATCHES DEGREES OF FREEDOM
				if(sum(linkage[[i]][['link.names']] %in% names(input_parameters)) < linkage[[i]][['dof']]) next
				
				# WHICH OF THE INPUT PARAMETERS WILL BE USED
				ip_idx <- which(names(input_parameters) %in% linkage[[i]][['link.names']])

				# NAME OF INPUT PARAMETER LINK
				ip_name <- names(input_parameters)[ip_idx]

				if(linkage[[i]][['dof']] == 1){

					ip_idx <- ip_idx[1]
					ip_name <- ip_name[1]

					# FIND THE POSITION OF THE INPUT LINK IN THE LINKAGE CHAIN

					# CORRESPONDING LINK NUMBER IN LINKAGE TO BE ANIMATED
					input_link_num <- which(linkage[[i]][['link.names']] == ip_name)[1]

					if(input_link_num == 1){

						# INPUT IS FIRST LINK - ANIMATE LINKAGE
						linkage_anim <- animateLinkage(linkage[[i]], input_parameters[[ip_name]])
					
						# INDICES OF LAST LINK JOINTS
						p_idx <- c(nrow(linkage[[i]][['joints']]), (nrow(linkage[[i]][['joints']])-1))

						# GET LINK NAME
						olink_name <- linkage[[i]][['link.names']][nrow(linkage[[i]][['joints']])-1]

					}else if(input_link_num == nrow(linkage[[i]][['joints']])-1){

						# INPUT IS OUTPUT LINK - REVERSE LINKAGE
						reverse_linkage <- reverseLinkage(linkage[[i]])

						# ANIMATE LINKAGE
						linkage_anim <- animateLinkage(reverse_linkage, input_parameters[[ip_name]])

						# INDICES OF FIRST LINK JOINTS (LINKAGE HAS BEEN REVERSED)
						p_idx <- c(nrow(linkage[[i]][['joints']]), (nrow(linkage[[i]][['joints']])-1))
					
						# GET LINK NAME
						olink_name <- linkage[[i]][['link.names']][1]
					}

					# GET COORDINATES OF OTHER LINK TO SAVE AS POTENTIAL SUBSEQUENT INPUT PARAMETER
					# GET INITIAL OUTLINK VECTOR
					p1_init <- linkage_anim$joints.initial[p_idx[1], ]
					p2_init <- linkage_anim$joints.initial[p_idx[2], ]
				
					# GET SUBSEQUENT OUTLINK VECTOR POSITIONS
					p1_change <- matrix(linkage_anim$joints[p_idx[1], ,], ncol=3, byrow=T)
					p2_change <- matrix(linkage_anim$joints[p_idx[2], , ], ncol=3, byrow=T)

					# GET CONSTRAINT VECTOR FOR LINK
					cvec <- linkage_anim$joints.cvec[p_idx[1], ]

					# GET T
					if(linkage[[i]][['joints.type']][nrow(linkage[[i]][['joints']])] == "R"){

						t_prev <- rep(NA, nrow(p2_change))

						for(rn in 1:nrow(p1_change)){

							# GET ANGLE BETWEEN INITIAL VECTOR AND ROTATED VECTOR
							a_vector <- avectors(p2_init-p1_init, p2_change[rn, ]-p1_change[rn, ])
						
							# FIND NEW POSITION FOR POSITIVE AND NEGATIVE ANGLE
							new_pos_p <- rotateBody(m=p2_init, p=p1_init, v=cvec, a=a_vector)
							new_pos_n <- rotateBody(m=p2_init, p=p1_init, v=cvec, a=-a_vector)

							# MAKE SURE THAT ANGLE HAS THE CORRECT SIGN
							if(abs(distPointToPoint(new_pos_p, p2_change[rn, ])) > abs(distPointToPoint(new_pos_n, p2_change[rn, ]))) a_vector <- -a_vector

							t_prev[rn] <- a_vector
						}
					}

					# SAVE AS INPUT PARAMETERS
					input_parameters[[olink_name]] <- t_prev

				}else if(linkage[[i]][['dof']] == 2){

					if(linkage[[i]][['link.names']][1] != ip_name[1] || linkage[[i]][['link.names']][3] != ip_name[2])
						stop("Temp fix error - not yet able to work with all 2 DOF linkages.")

					# TEMPORARY FIX - TO START ALLOWING 2 DOF LINKAGES
					linkage_anim <- animateLinkage(linkage[[i]], 
						t=cbind(input_parameters[[ip_name[1]]], input_parameters[[ip_name[2]]]))
				}
				
				# SAVE ANIMATED LINKAGE
				linkage_r[[i]] <- linkage_anim
		
				if(!is.null(linkage_anim$points)){

					# MAKE SYSTEM LEVEL POINTS AN ARRAY IF MATRIX
					if(length(dim(points)) == 2)
						points <- array(points, dim=c(dim(points), dim(linkage_anim$points)[3]), dimnames=list(rownames(points), NULL, NULL))

					# ONLY COPY POINTS FROM LINKS IN LINKAGE
					to_copy <- unlist(linkage_anim$points.assoc[linkage_anim$link.names])
					
					# COPY ANIMATED POINTS INTO SYSTEM LEVEL POINT ARRAY
					points[rownames(linkage_anim$points[to_copy, , ]), , ] <- linkage_anim$points[to_copy, , ]
				}
				
				#print(input_parameters)
				#cat('-----------\n')

				animated[i] <- TRUE
			}

			n <- n + 1
		}
		
		linkage_r$points <- points
		linkage_r$link.names <- link.names
		linkage_r$link.assoc <- link.assoc
		linkage_r$points.assoc <- points.assoc

		return(linkage_r)
	}
	
	check.inter.joint.dist <- TRUE
	check.inter.point.dist <- TRUE

	# CHECK THAT NUMBER OF INPUTS MATCHES LINKAGE DEGREES OF FREEDOM
	if(linkage$dof == 2 && is.vector(t)) 
		stop("Input 't' is a vector. For a linkage with two degrees of freedom, 't' must be two column matrix, where each column corresponds to each input link.")

	# CONVERT T INTO MATRIX FOR CONSISTENCY ACROSS LINKAGES WITH DIFFERING DEGREES OF FREEDOM
	if(!is.matrix(t)) t <- matrix(t, nrow=length(t), ncol=1)

	# LINKAGE ARRAY FOR POINTS IF DEFINED
	if(!is.null(linkage$points)){

		# CONVERT ARRAY TO MATRIX - COPY OVER LAST DIMENSION OF ARRAY
		if(length(dim(linkage$points)) == 3) linkage$points <- linkage$points[, , dim(linkage$points)[3]]
	}

	# CONVERT ARRAY TO MATRIX - COPY OVER LAST DIMENSION OF ARRAY
	if(length(dim(linkage$joints)) == 3) linkage$joints <- linkage$joints[, , dim(linkage$joints)[3]]

	# NEW LINKAGE OBJECT
	linkage_r <- linkage

	# COPY ORIGINAL JOINT POSITIONS
	linkage_r$joints.initial <- linkage_r$joints

	# COPY OVER JOINTS AND POINTS
	linkage_r$joints <- array(linkage$joints, dim=c(nrow(linkage$joints), ncol(linkage$joints), nrow(t)), dimnames=list(rownames(linkage$joints), colnames(linkage$joints), NULL))
	if(!is.null(linkage$points)) linkage_r$points <- array(linkage$points, dim=c(nrow(linkage$points), ncol(linkage$points), nrow(t)), dimnames=list(rownames(linkage$points), colnames(linkage$points), NULL))

	# GET LINK LENGTHS
	link_len <- distPointToPoint(linkage$joints)

	# SET LINK NAMES
	if(!is.null(linkage$link.names)){link_names <- linkage$link.names}else{link_names <- names(linkage$points.assoc)}
	
	# ADD END JOINT TYPE FOR INTERNAL CONSISTENCY
	linkage$joints.type <- c(linkage$joints.type, "E")
	
	for(i in 1:nrow(t)){

		for(j in 1:nrow(linkage$joints)){
		
			# SET INPUT LINK NUMBER
			curr.input <- 1
		
			# TRANSLATE FIRST JOINT WITH INPUT ALONG CONSTRAINT VECTOR
			if(j == 1 && linkage$joints.type[j] == 'L')
				linkage_r$joints[j, , i] <- linkage_r$joints[j, , i] + t[i, curr.input] * linkage$joints.cvec[j, ]

			# ROTATE SECOND JOINT ABOUT FIRST
			if(j == 1 && linkage$joints.type[j] == 'R')
				linkage_r$joints[j+1, , i] <- rotateBody(m=linkage_r$joints[j+1, , i], p=linkage_r$joints[j, , i], 
					v=linkage$joints.cvec[j, ], a=t[i, curr.input])

			# JOINT MOVES ALONG LINE
			if(j > 1 && linkage$joints.type[j] == 'L'){
			
				# GET POINT FOR COMPARISON FROM PREVIOUS ITERATION
				if(i == 1){point_compare <- linkage_r$joints[j, , i]}else{point_compare <- linkage_r$joints[j, , i-1]}

				# FIND POSITION OF SLIDING JOINT
				linkage_r$joints[j, , i] <- intersectSphereLine(c=linkage_r$joints[j-1, , i], r=link_len[j-1], 
					x=linkage_r$joints[j, , i], l=linkage$joints.cvec[j, ], point.compare=point_compare)
			}

			# IF PREVIOUS JOINT IS R, ROTATE ABOUT PREVIOUSLY ROTATED JOINT
			if(j > 1 && linkage$joints.type[j-1] == 'U' && linkage$joints.type[j] == 'R' && linkage$joints.type[j+1] == 'R'){

				# NEXT INPUT LINK
				curr.input <- curr.input + 1
				
				# ROTATE INPUT LINK AND PRECEDING R JOINT
				linkage_r$joints[j:(j-1), , i] <- rotateBody(m=linkage$joints[j:(j-1), ], p=linkage$joints[j+1, ], 
					v=linkage$joints.cvec[j+1, ], a=t[i, curr.input])
			}

			# POINT IN PLANE AT DISTANCE FROM ROTATED JOINT
			if(j > 1 && linkage$joints.type[j] == 'P' && linkage$joints.type[j+1] == 'U' && linkage$joints.type[j+2] == 'R'){
			
				# NEXT INPUT LINK
				curr.input <- curr.input + 1
				
				# ROTATE INPUT LINK AND PRECEDING R JOINT
				linkage_r$joints[j+1, , i] <- rotateBody(m=linkage$joints[j+1, ], p=linkage$joints[j+2, ], 
					v=linkage$joints.cvec[j+2, ], a=t[i, curr.input])

				# GET POINT FOR COMPARISON FROM PREVIOUS ITERATION
				if(i == 1){compare <- linkage$joints[j, ]}else{compare <- linkage_r$joints[j, , i-1]}

				# SOLVE FOR POINT POSITION IN PLANE
				linkage_r$joints[j, , i] <- pointOnPlaneFromPoints(p=linkage_r$joints[j, , i], n=linkage$joints.cvec[j, ], 
					p1=linkage_r$joints[j-1, , i], d1=distPointToPoint(linkage$joints[c(j,j-1), ]), 
					p2=linkage_r$joints[j+1, , i], d2=distPointToPoint(linkage$joints[c(j,j+1), ]),
					compare=compare)[[1]]
			}

			# JOINT ROTATES
			if(j > 1 && linkage$joints.type[j] == 'R' && linkage$joints.type[j-1] == 'U' && linkage$joints.type[j-2] %in% c('U', 'L')){

				# GET POINT FOR COMPARISON FROM PREVIOUS ITERATION
				if(i == 1){point_compare <- linkage$joints[j-1, ]}else{point_compare <- linkage_r$joints[j-1, , i-1]}

				# DEFINE CIRCLE FOR OUTPUT LINK
				output_circle <- defineCircle(center=linkage_r$joints[j, , i], nvector=linkage$joints.cvec[j, ], 
					point_on_radius=linkage_r$joints[j-1, , i])

				# FIND ANGLE ON CIRCLE AT DISTANCE FROM TRANSMISSION LINK JOINT
				output_link_t <- angleOnCircleFromPoint(circle=output_circle, dist=link_len[j-2], 
					P=linkage_r$joints[j-2, , i], point_compare=point_compare)

				# FIND CORRESPONDING POINT ON CIRCLE
				output_joint_r <- circlePoint(circle=output_circle, T=output_link_t)

				# FIND ROTATION ANGLE FOR OUTLINK
				t_transform <- avectors(linkage_r$joints[j-1, , i] - output_circle$C, output_joint_r - output_circle$C)
	
				# ROTATE TRANSMISSION LINK-OUTPUT JOINT
				joint_npos <- rotateBody(m=linkage_r$joints[j-1, , i], p=linkage_r$joints[j, , i],
					v=linkage$joints.cvec[j, ], a=t_transform)

				# CHECK THAT ROTATION WAS IN THE RIGHT DIRECTION
				if(abs(distPointToPoint(linkage_r$joints[j-2, , i], joint_npos) - link_len[j-2]) > 1e-4){
					t_transform <- -t_transform
					joint_npos <- rotateBody(m=linkage_r$joints[j-1, , i], p=linkage_r$joints[j, , i], 
						v=linkage$joints.cvec[j, ], a=t_transform)
				}

				linkage_r$joints[j-1, , i] <- joint_npos
			}
			
			# TRANSFORM POINTS ASSOCIATED WITH EACH LINK
			if(!is.null(linkage_r$points)){
			
				# SKIP FIRST STEP, MAKE SURE BOTH POINTS OF LINK HAVE BEEN TRANSFORMED
				if(j == 1) next

				# USE INPUT T FOR INPUT LINK
				if(j == 2) t_transform <- t[i, curr.input]

				# ROTATE LINK ABOUT FIRST JOINT (RU)
				if(paste(linkage$joints.type[(j-1):j], collapse="") %in% c('RU')){
					linkage_r$points[linkage$points.assoc[[link_names[j-1]]], , i] <- rotateBody(
						m=linkage$points[linkage$points.assoc[[link_names[j-1]]], ], 
						p=linkage$joints[j-1, ], v=linkage$joints.cvec[j-1, ], a=t_transform)
				}

				# COPY TRANSFORMATION (LL, UL)
				if(paste(linkage$joints.type[(j-1):j], collapse="") %in% c('LL', 'UL')){
					linkage_r$points[linkage$points.assoc[[link_names[j-1]]], , i] <- copyTransformation(
						m1=linkage$joints[c(j-1,j), ], m2=linkage_r$joints[c(j-1,j), , i], 
						mn=linkage$points[linkage$points.assoc[[link_names[j-1]]], ])
				}

				# COPY TRANSFORMATION (LU_, UU_, LP_, PU_)
				if(paste(linkage$joints.type[(j-2):(j-1)], collapse="") %in% c('LU', 'UU', 'LP', 'PU')){
					linkage_r$points[linkage$points.assoc[[link_names[j-2]]], , i] <- copyTransformation(
						m1=linkage$joints[c(j-2,j-1), ], m2=linkage_r$joints[c(j-2,j-1), , i], 
						mn=linkage$points[linkage$points.assoc[[link_names[j-2]]], ])
				}

				# ROTATE LINK ABOUT SECOND JOINT WITH INPUT ROTATION (URR)
				if(paste(linkage$joints.type[(j-1):(j+1)], collapse="") %in% c('URR')){

					linkage_r$points[linkage$points.assoc[[link_names[j]]], , i] <- rotateBody(
						m=linkage$points[linkage$points.assoc[[link_names[j]]], ], 
						p=linkage$joints[j+1, ], v=linkage$joints.cvec[j+1, ], a=t[i, curr.input])

					linkage_r$points[linkage$points.assoc[[link_names[j-1]]], , i] <- rotateBody(
						m=linkage$points[linkage$points.assoc[[link_names[j-1]]], ], 
						p=linkage$joints[j+1, ], v=linkage$joints.cvec[j+1, ], a=t[i, curr.input])
				}
				

				# ROTATE LINK ABOUT SECOND JOINT (UR)
				if(paste(linkage$joints.type[(j-1):j], collapse="") %in% c('UR')){
					linkage_r$points[linkage$points.assoc[[link_names[j-1]]], , i] <- rotateBody(
						m=linkage_r$points[linkage$points.assoc[[link_names[j-1]]], , i], 
						p=linkage_r$joints[j, , i], v=linkage$joints.cvec[j, ], a=t_transform)
				}
			}
		}
	}

	# CHECK THAT DISTANCES WITHIN LINKS HAVE NOT CHANGED
	if(check.inter.joint.dist && dim(linkage_r$joints)[3] > 1){
	
		linkage_size <- distPointToPoint(colMeans(linkage$joints), linkage$joints)
		t_size <- mean(colMeans(abs(t)))
		
		for(i in 1:(dim(linkage_r$joints)[1]-1)){

			# GET DISTANCES BETWEEN CONSECUTIVE JOINT POSITIONS OVER TIME
			d <- distPointToPoint(t(linkage_r$joints[i, , ]), t(linkage_r$joints[i+1, , ]))

			# ALL DISTANCES CONSTANT
			if(abs(sd(d) / t_size) < 1e-7 && abs(sd(d) / linkage_size) < 1e-7) next

			# PRINT DISTANCES THAT CHANGE
			warning(paste0("Link lengths are non-constant (", linkage$link.names[i], ": ", sd(d), ")."))
		}
	}

	# CHECK THAT DISTANCES WITHIN LINKS HAVE NOT CHANGED
	if(check.inter.point.dist && !is.null(linkage_r$points) && dim(linkage_r$points)[3] > 1){
		for(points_assoc in linkage$points.assoc){

			if(length(points_assoc) < 2) next
		
			# GET ALL POINTS ASSOCIATED WITH BODY
			n <- linkage_r$points[points_assoc, , ]

			# GENERATE PAIRS
			r1 <- r2 <- c(1,dim(n)[1])
			p <- matrix(NA, nrow=0, ncol=2)
			for(i in r1[1]:r1[2]){
				for(j in r2[1]:r2[2]){
					if(j < i && r2[2] >= r1[2]){next}
					if(i < j && r2[2] < r1[2]){next}
					if(j == i){next}

					p <- rbind(p, c(i, j))
				}
			}

			# DISTANCE MATRIX
			d <- matrix(NA, nrow=nrow(p), ncol=dim(n)[3])

			# FIND DISTANCES BETWEEN PAIRS OF POINTS
			for(j in 1:dim(n)[3]) d[, j] <- distPointToPoint(n[p[, 1], , j], n[p[, 2], , j])

			# FIND SD OF EACH ROW
			d_sd <- apply(d, 1, sd)
		
			# ALL DISTANCES CONSTANT
			if(sum(na.omit(d_sd) > 1e-8) == 0) next

			# PRINT DISTANCES THAT CHANGE
			warning("Interpoint distance within link are non-constant.")
		}
	}

	linkage_r
}
