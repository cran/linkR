\name{defineLinkage}
\alias{defineLinkage}
\title{Defines a lever or linkage}
\description{
This function takes the joint positions, types and constraints of a lever or linkage mechanism or a set of minimum parameters and creates a list of class \code{"linkage"} that forms the basic object for model creation and analysis.
}
\usage{
defineLinkage(joint.coor, joint.types, joint.cons, 
              joint.conn = NULL, points = NULL, link.assoc = NULL, 
              link.names = NULL, ground.link = NULL, lar.cons = NULL)
}
\arguments{
  \item{joint.coor}{A matrix of 2D or 3D coordinates that are the joints of a lever or linkage mechanism.}
  \item{joint.types}{A vector of single letters indicating the type of constraints acting on each joint. Supported types are "R", "U", "L" and "P". See Details.}
  \item{joint.cons}{A list or matrix of 3D constraint vectors that correspond to each joint.}
  \item{joint.conn}{A two-column matrix having the same number of rows as \code{joint.coor}, specifying the two links that each joint connects.}
  \item{points}{A matrix of points associated with one or more of the links in the linkage (optional).}
  \item{link.assoc}{A vector of integers or rownames indicating the link with which each row in \code{points} is associated. Required if \code{points} is non-\code{NULL} but otherwise optional.}
  \item{link.names}{A vector of the link names (optional).}
  \item{ground.link}{A numeric or link name specifying the ground link of the linkage.}
  \item{lar.cons}{A list specifying long-axis rotation constraints on links with S-joints on each end. This feature will eventually be added into the \code{joint.conn} input parameter as network connections.}
}
\details{
This function creates a linkage network, a list structure of class \code{"linkage"}, from a matrix of joint coordinates, constraints and interconnections. This list structure can be input into functions such as animateLinkage or drawLinkage.

The motion of each joint in the linkage and the nature of force and torque transmission at the joint depend on the constraint acting on each joint. \code{linkR} currently supports four joint types:

\itemize{
	\item \bold{R} : Joint constraint is rotational. Allows rotation about a single, fixed axis (hinge joint).
	\item \bold{S} : Joint is spherical. Allows rotation about any axis (ball-and-socket joint) including long-axis rotation.
	\item \bold{L} : Joint constraint is linear. Joint translates along a vector of fixed orientation (slides along a line).
	\item \bold{P} : Joint constraint is planar. Joint translates along a plane of fixed orientation (slides within a plane).
}

Please see \href{http://home.uchicago.edu/~aolsen/software/linkr.shtml}{linkR Resources} for a tutorial on how to use the linkR package and the \href{http://home.uchicago.edu/~aolsen/software/linkr/examples.shtml}{linkR example gallery} for several examples (code and animations) of different linkages that can be created using linkR.
}
\value{

		'dof' = dof

a list of class \code{"linkage"} with the following elements:
	\item{joint.coor}{A matrix of 2D or 3D coordinates that are the joints of a lever or linkage mechanism.}
	\item{joint.cons}{A vector of single letters indicating the type of constraints acting on each joint.}
	\item{joint.types}{A list or matrix of 3D constraint vectors that correspond to each joint.}
	\item{joint.links}{A matrix specifying the joints connecting to each link. Used by the \code{animateLinkage} function.}
	\item{joint.paths}{"Path fragments" along the linkage network, used by the \code{animateLinkage} function to solve for the position of unresolved joints.}
	\item{joint.conn}{A two-column matrix having the same number of rows as \code{joint.coor}, specifying the two links that each joint connects.}
	\item{joint.init}{The initial position of the joints in the linkage.}
	\item{ground.joints}{The ground joints in the linkage.}
	\item{points}{A matrix of points associated with any of the links in the linkage.}
	\item{points.assoc}{The links with which each point is associated.}
	\item{link.assoc}{A vector of integers or rownames indicating the link with which each row, if input.}
	\item{link.names}{A vector of the link names.}
	\item{link.lcs}{Local coordinate systems assigned to each link which will be transformed with the associated link for kinematic analysis.}
	\item{lar.cons}{Constraints on long-axis rotation.}
	\item{num.links}{The number of links in the linkage.}
	\item{dof}{The number of degrees of freedom of the linkage. Currently, this may not be accurately estimated for all linkages.}
}
\seealso{\code{\link{animateLinkage}}}
\author{Aaron Olsen}
\examples{
## Define and animate coupled planar and linear sliders
# Joint coordinates
joint.coor <- rbind(c(-0.6,0,0), c(-0.2,1,0), c(0,1,0), c(0.2,1,0), 
	c(1.5,0.5,0), c(1.5,0.4,0))

# Joint types
joint.types <- c("S", "S", "P", "S", "S", "L")

# Joint constraint vectors
joint.cons <- list(NA, NA, c(1,0,0), NA, NA, c(1,0,0))

# The links connected by each joint
joint.conn <- rbind(c(0,1), c(1,2), c(2,0), c(2,3), c(3,4), c(4,0))

# Input parameters, with 100 iterations
input.param <- seq(0, -0.5, length=5)

# Joint at which to apply input parameters
input.joint <- 6

# Add points to clarify the visualization
points <- rbind(c(-0.4,0.5,0), c(1.4,0.4,0), c(1.4,0.5,0), c(1.6,0.5,0), 
	c(1.6,0.4,0), c(0.75,0.4,0), c(1.75,0.4,0), c(0.85,0.75,0),
	c(-0.2,0.9,0.1), c(0.2,0.9,0.1), c(0.2,0.9,-0.1), c(-0.2,0.9,-0.1),
	c(-0.2,1.1,0.1), c(0.2,1.1,0.1), c(0.2,1.1,-0.1), c(-0.2,1.1,-0.1))

# The link each point is associated with
link.assoc <- c(1,4,4,4,4,0,0,3,rep(2,8))

# Lines to connect points for easier visualization
path.connect <- list(c(2:5,2), c(6:7), c(9:12,9), c(13:16,13), c(9,13), c(10,14), 
	c(11,15), c(12,16))

# Define the linkage
linkage <- defineLinkage(joint.coor=joint.coor, joint.types=joint.types, 
	joint.cons=joint.cons, joint.conn=joint.conn, points=points, link.assoc=link.assoc)

# Animate the linkage
animate <- animateLinkage(linkage, input.param=input.param, input.joint=input.joint)

# Draw the linkage
# This will create a figure using the plot device
# Refer to the linkR tutorial for how to create interactive 3D animations (see "Details")
drawLinkage(animate, path.connect=path.connect)
}