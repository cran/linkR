\name{animateLinkage}
\alias{animateLinkage}
\title{Animates a lever or linkage}
\description{
This function animates a lever or linkage mechanism according to specified translational or rotational motion of the input link.
}
\usage{
animateLinkage(linkage, input.param, input.joint=NULL,
               check.inter.joint.dist = TRUE, check.joint.cons = TRUE, 
               check.inter.point.dist = TRUE)
}

\arguments{
  \item{linkage}{A linkage object. This is a list of class \code{"linkage"} created by \code{\link{defineLinkage}}.}
  \item{input.param}{A list containing the input parameters for the linkage. The number of list elements corresponds to the number of joints for which input motion is specified. Thus, a one degree-of-freedom mechanism should be of length one; a two degree-of-freedom mechanism of length two, etc.}
  \item{input.joint}{A vector specifying the joint(s) at which the input motion is to be applied. Currently, input motion can only be specified at grounded joints.}
  \item{check.inter.joint.dist}{A logical indicating whether a check should be run to verify that the distances among connected joints is constant. A warning message is displayed if the distances are non-constant.}
  \item{check.joint.cons}{A logical indicating whether a check should be run to verify that the joint constraints are maintained through the simulation. This feature is currently in development and does not work for all joint types.}
  \item{check.inter.point.dist}{A logical indicating whether a check should be run to verify that the distances among points on the same body are constant. A warning message is displayed if the distances are non-constant.}
}
\details{
Please see \href{http://home.uchicago.edu/~aolsen/software/linkr.shtml}{linkR Resources} for a tutorial on how to use the linkR package and the \href{http://home.uchicago.edu/~aolsen/software/linkr/examples.shtml}{linkR example gallery} for several examples (code and animations) of different linkages that can be created using linkR.
}
\value{
a list of class \code{"linkage"} having the same elements as the input \code{linkage} with some exceptions, including:
	\item{joint.coor}{The joint matrix will be converted to an array, where \code{dim(joints)[3]} is equal to the number of animation iterations.}
	\item{points}{If provided in the input, the point matrix will be converted to an array, where \code{dim(points)[3]} is equal to the number of animation iterations.}
}
\seealso{\code{\link{defineLinkage}}}
\author{Aaron Olsen}
\examples{
## Define and simulate a slider along a rotating link
# Joint coordinates
joint.coor <- rbind(c(0,0,0), c(0,0.75,0), c(0.1,0.75,0), c(0.8,0.4,0))

# Joint types
joint.types <- c("R", "L", "S", "S")

# Joint constraint vectors
joint.cons <- list(c(0,0,1), c(0,1,0), NA, NA)

# Names of each link
link.names <- c('Ground', 'Link1', 'Link2', 'Link3')

# The links connected by each joint
joint.conn <- rbind(c('Ground', 'Link1'), c('Link1', 'Link2'), 
	c('Link2', 'Link3'), c('Link3', 'Ground'))

# Input parameters, with 100 iterations
input.param <- seq(from=0, to=0.5, length=5)

# Joint at which to apply input parameters
input.joint <- 1

# Add points to clarify the visualization
points <- rbind(c(0,0.65,0), c(0.1,0.65,0), c(0.1,0.85,0), c(0,0.85,0), 
	c(0,0,0), c(-0.1,0,0), c(-0.1,1.5,0), c(0,1.5,0), c(0.45, 0.575, 0))

# The link each point is associated with
link.assoc <- c(2,2,2,2,1,1,1,1,3)

# Lines to connect points for easier visualization
path.connect <- list(c(1:4,1), c(5:8,5))

# Define the linkage
linkage <- defineLinkage(joint.coor=joint.coor, joint.types=joint.types, 
	joint.cons=joint.cons, joint.conn=joint.conn, link.names=link.names, 
	points=points, link.assoc=link.assoc)

# Animate the linkage
animate <- animateLinkage(linkage, input.param=input.param, input.joint=input.joint)

# Draw the linkage
# This will create a figure using the plot device
# Refer to the linkR tutorial for how to create interactive 3D animations (see "Details")
drawLinkage(animate, path.connect=path.connect)
}