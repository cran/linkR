reverseLinkage <- function(linkage){
	
	# REVERSE JOINTS
	linkage$joints <- linkage$joints[nrow(linkage$joints):1, ]
	rownames(linkage$joints) <- rownames(linkage$joints)[nrow(linkage$joints):1]

	# REVERSE JOINT CONSTRAINTS
	linkage$joints.cvec <- linkage$joints.cvec[nrow(linkage$joints.cvec):1, ]
	rownames(linkage$joints.cvec) <- rownames(linkage$joints.cvec)[nrow(linkage$joints.cvec):1]

	# REVERSE JOINT TYPES
	linkage$joints.type <- linkage$joints.type[length(linkage$joints.type):1]

	# REVERSE LINK NAMES
	if(!is.null(linkage$link.names)) linkage$link.names <- c(linkage$link.names[(length(linkage$link.names)-1):1], linkage$link.names[length(linkage$link.names)])
	
	# REVERSE LINK-POINT ASSOCIATIONS
	if(!is.null(linkage$points.assoc) && is.null(names(linkage$points.assoc))) linkage$points.assoc <- linkage$points.assoc[length(linkage$points.assoc):1]

	# GET REVERSE MINIMUM PARAMETERS
	linkage$min.param <- defineLinkage(joints=linkage$joints, joints.type=linkage$joints.type, joints.cvec=linkage$joints.cvec)$min.param

	linkage
}