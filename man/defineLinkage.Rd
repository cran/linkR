\name{defineLinkage}
\alias{defineLinkage}
\title{Defines a lever or linkage}
\description{
This function takes the joint positions, types and constraints of a lever or linkage mechanism or a set of minimum parameters and creates a list of class \code{"linkage"} that forms the basic object for model creation and analysis.
}
\usage{
defineLinkage(joints = NULL, joints.type = NULL, joints.cvec = NULL, 
              min.param = NULL, points = NULL, link.assoc = NULL, 
              link.names = NULL, euler.vec = c(1,0,0))
}
\arguments{
  \item{joints}{A matrix of 2D or 3D coordinates that are the joints of a lever or linkage mechanism.}
  \item{joints.type}{A vector of single letters indicating the type of constraints acting on each joint. Supported types are "R", "U", "L" and "P". See Details.}
  \item{joints.cvec}{A list or matrix of 3D constraint vectors that correspond to each joint.}
  \item{min.param}{An alternative input from \code{joints} in the form of a vector or list of minimum parameters that define a given linkage. Currently, this type of input is only supported for linkages of the type "RUL", "RUUR" and "RULUR".}
  \item{points}{A matrix of points associated with one or more of the links in the linkage (optional).}
  \item{link.assoc}{A vector of integers or rownames indicating the link with which each row in \code{points} is associated. Required if \code{points} is non-\code{NULL} but otherwise optional.}
  \item{link.names}{A vector of the link names (optional).}
  \item{euler.vec}{A reference vector used by \code{defineLinkage} in finding the Euler rotation angles for minimum parameters.}
}
\details{
This function creates a list structure of class \code{"linkage"} from either a matrix of joint coordinates or a vector of minimum parameters that contains a complete description of the linkage's geometry and the constraints on its movement. This list structure can be input into functions such as animateLinkage or drawLinkage.

The motion of each joint in the linkage and the nature of force and torque transmission at the joint depend on the constraint acting on each joint. \code{linkR} currently supports four joint types:

\itemize{
	\item \bold{R} : Joint constraint is rotational. Allows rotation about a single, fixed axis (hinge joint).
	\item \bold{U} : Joint is unconstrained. Allows rotation about any axis (ball-and-socket joint).
	\item \bold{L} : Joint constraint is linear. Joint translates along a vector of fixed orientation (slides along a line).
	\item \bold{P} : Joint constraint is planar. Joint translates along a plane of fixed orientation (slides within a plane).
}

These joint types can be combined to create a linkage mechanism. \code{linkR} currently supports 10 different combinations with the indicated degrees of freedom (DOF):

\itemize{
	\item \bold{LL} : Two coupled joints that slide along specified vectors (1 DOF).
	\item \bold{LLL} : Three coupled joints that slide along specified vectors (1 DOF).
	\item \bold{RU} : A lever that rotates about a fixed axis of rotation (1 DOF).
	\item \bold{LUR}, \bold{RUL} : A sliding joint connected to a rotating lever through a single coupling joint (1 DOF).
	\item \bold{LLUR} : Two sliding joints connected to a rotating lever through a single coupling joint (1 DOF).
	\item \bold{RUUR} : A standard 4-bar linkage, with two rotating links connected by a coupling link (1 DOF).
	\item \bold{RULUR} : A 5-bar linkage, with two rotating links connected by two coupling links that share a sliding joint (1 DOF).
	\item \bold{LURR} : A 3-bar linkage in which a sliding joint is connected to a rotating link through another rotating link (2 DOF).
	\item \bold{LPUR} : A 3-bar linkage in which a sliding joint is connected to a rotating link through a coupling link, one joint of which slides within a plane (2 DOF).
}

For additional examples, see: \href{http://home.uchicago.edu/~aolsen/software/linkr/examples.shtml}{linkR Example Gallery}
}
\value{
a list of class \code{"linkage"} with the following elements:
	\item{joints}{A matrix of 2D or 3D coordinates that are the joints of a lever or linkage mechanism.}
	\item{joints.cvec}{A vector of single letters indicating the type of constraints acting on each joint.}
	\item{joints.type}{A list or matrix of 3D constraint vectors that correspond to each joint.}
	\item{points}{A matrix of points associated with one or more of the links in the linkage, if input.}
	\item{points.assoc}{A list of the same length as the number of links (\code{nrow(joints)}) indicating the points associated with each link.}
	\item{link.assoc}{A vector of integers or rownames indicating the link with which each row, if input.}
	\item{link.names}{A vector of the link names.}
	\item{min.param}{A vector of minimum parameters for the particular linkage type. \code{NULL} for types other than "RUL", "RUUR" and "RULUR".}
	\item{dof}{The number of degrees of freedom of the linkage.}
}
\seealso{\code{\link{animateLinkage}}}
\author{Aaron Olsen}
\examples{

###                      Defining a linkage of type RUUR (1 DOF)                    ###
## Set the joint coordinates
joints <- matrix(c(0,0,0, 0,2,0, 1,1,0, 1,0,0), nrow=4, ncol=3, byrow=TRUE)

## Set the joint constraint vectors
joints.cvec <- list(c(1,0,0), c(1,0,0), NA, c(0,0,1))

## Define the linkage
linkage <- defineLinkage(joints=joints, joints.cvec=joints.cvec, 
    joints.type=c("R", "U", "U", "R"))

## Animate the linkage
link_anim <- animateLinkage(linkage, t=seq(0, 0.2, length=50))

## Draw the linkage using native R plotting
drawLinkage(link_anim, method="plot")


###                      Defining a linkage of type LPUR (2 DOF)                    ###
## Set the joint coordinates
joints <- matrix(c(0,0,0, 1,1,0, 1,2,0, 2,3,0), nrow=4, ncol=3, byrow=TRUE)

## Set points to associate with each link (positioned in the middle of each link)
points <- rbind(
    colMeans(joints[c(1,2), ]), 
    colMeans(joints[c(2,3), ]), 
    colMeans(joints[c(3,4), ]))

## Set the joint constraint vectors
joints.cvec <- list(c(1,0,0), c(1,0,0), NA, c(0,0,1))

## Define the linkage
linkage <- defineLinkage(joints=joints, joints.cvec=joints.cvec, 
    joints.type=c("L", "P", "U", "R"), points=points, link.assoc=1:3)

## Animate the linkage
link_anim <- animateLinkage(linkage, t=cbind(seq(0, 0.3, 0.01), seq(0, -0.3, -0.01)))

## Draw the linkage using native R plotting
drawLinkage(link_anim, method="plot")

}