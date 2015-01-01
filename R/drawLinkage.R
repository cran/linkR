drawLinkage <- function(linkage, method = "svgViewR", file = NULL, animate = TRUE, animate.duration = 1, animate.reverse = FALSE, 
	animate.repeat = -1, path.connect=NULL, 
	joint.col.fill="white", joint.col.stroke="black", joint.cex=4, joint.lwd=2,
	point.col.fill="black", point.col.stroke="black", point.cex=2, point.lwd=2,
	path.col.fill="none", path.opacity.fill=1, path.col.stroke="black", path.lwd=1){

	if(method == "plot"){

		joints2d <- proj3DTo2D(linkage$joints)
		
		# CONVERT ARRAY TO MATRIX OF ALL POINTS
		all_points <- apply(joints2d, 2, rbind)
		
		# CREATE PLOT DEVICE THAT FITS ALL JOINT COORDINATES
		# BUG: EXPAND TO INCLUDE OTHER POINTS ASSOCIATED WITH LINKS
		plot(all_points)

		for(i in 1:dim(joints2d)[3]){

			# PLOT JOINTS
			points(joints2d[, , i])

			# PLOT CONNECTING LINE SEGMENTS
			for(j in 1:(dim(joints2d)[1]-1)) segments(joints2d[j, 1, i], joints2d[j, 2, i], joints2d[j+1, 1, i], joints2d[j+1, 2, i])
		}
	}

	if(method == "svgViewR"){

		points <- linkage$points

		if(class(linkage) == 'linkage_system'){
			linkage_system <- linkage
		}else{
			linkage_system <- list(linkage)
		}
		
		# CREATE NEW SVG FILE
		svgviewr.new(file=file, window.title='Linkage Viewer', animate.duration=animate.duration, animate.reverse=animate.reverse, animate.repeat)

		# VECTOR OF DRAWN LINKS
		drawn_links <- c()

		# INITIAL PATH INDEX
		index.add <- 0

		# GET NUMBER OF LINKAGES
		linkage_ct <- sum(names(linkage_system) == "")

		for(linkage in linkage_system[1:linkage_ct]){

			# IF MATRIX, CONVERT TO ARRAY
			if(length(dim(linkage$joints)) == 2) linkage$joints <- array(linkage$joints, dim=c(dim(linkage$joints)[1], dim(linkage$joints)[2], 1))

			# COPY JOINTS
			joints <- linkage$joints

			# DRAW JOINTS
			svg_points <- svgviewr.points(joints, file=file, animate=animate, col.fill=joint.col.fill, col.stroke=joint.col.stroke, cex=joint.cex, lwd=joint.lwd)

			# CONNECT JOINTS WITH PATHS
			if(is.null(path.connect)){
				if(animate){
					svgviewr.pathsC(1:dim(joints)[1], file=file, index.add=index.add, lwd=path.lwd)
				}else{
					svgviewr.pathsC(matrix(1:(dim(joints)[1]*dim(joints)[3]), nrow=dim(joints)[3], ncol=dim(joints)[1], byrow=TRUE), file=file, index.add=index.add, lwd=path.lwd)
				}
			}

			# ADVANCE POINT COUNT FOR PATH INDEX
			if(animate){index.add <- index.add + dim(joints)[1]}else{index.add <- index.add + (dim(joints)[1]*dim(joints)[3])}
		}
		
		if(!is.null(points)){

			# DRAW POINTS
			svgviewr.points(points, file=file, animate=animate, col.fill=point.col.fill, col.stroke=point.col.stroke, cex=point.cex, lwd=point.lwd)

			# DRAW PATHS CONNECTING JOINTS
			if(!is.null(path.connect)){

				# CREATE PATH LIST TO CONNECT JOINTS
				path_list <- vector("list", length(path.connect))

				if(!is.null(dimnames(points)[[1]])){
					for(i in 1:length(path.connect)){
						for(j in 1:length(path.connect[[i]])){

							# PATH PATH LABELS TO ROWNAMES OF POINT ARRAY
							grepl_match <- grepl(path.connect[[i]][j], dimnames(points)[[1]])
							
							# ADD TO PATH LIST
							if(sum(grepl_match) > 0) path_list[[i]] <- c(path_list[[i]], which(grepl_match) + index.add)
						}
					}
				}else{
					path_list <- path.connect + index.add
				}

				# CONNECT POINTS WITH PATHS
				if(animate){
					svgviewr.pathsC(path_list, file=file, col.fill=path.col.fill, opacity.fill=path.opacity.fill, col.stroke=path.col.stroke, lwd=path.lwd)
				}else{
				}
			}
		}
	}
	
	1
}
