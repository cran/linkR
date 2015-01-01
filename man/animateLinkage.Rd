\name{animateLinkage}
\alias{animateLinkage}
\title{Animates a lever or linkage}
\description{
This function animates a lever or linkage mechanism according to specified translational or rotational motion of the input link.
}
\usage{
animateLinkage(linkage, t, input.link = NULL)
}
\arguments{
  \item{linkage}{A list of class \code{"linkage"}, such as that created by \code{\link{defineLinkage}}.}
  \item{t}{For a one degree-of-freedom linkage, a vector of distances or angles (in radians) over which the input link will be translated/rotated. For a two degree-of-freedom linkage, a matrix in which the first column is the input to the first input link and the second column is the input to the second input link.}
  \item{input.link}{The name of the link considered the input link. This is assumed to be the first link by default. For most linkage types, only the first link can be the input.link.}
}
\value{
a list of class \code{"linkage"} having the same elements as the input \code{linkage} with the following exceptions:
	\item{joints}{The joint matrix will be converted to an array, where \code{dim(joints)[3]} is equal to \code{length(t)} (number of animation iterations).}
	\item{points}{If provided in the input, the point matrix will be converted to an array, where \code{dim(points)[3]} is equal to \code{length(t)} (number of animation iterations).}
}
\seealso{\code{\link{defineLinkage}}}
\author{Aaron Olsen}
\examples{

###                         Defining a standard 4-bar linkage                       ###
## Set the joint coordinates
joints <- matrix(c(0,0,0, 0,2,0, 1,1,0, 1,0,0), nrow=4, ncol=3, byrow=TRUE)

## Set the joint constraint vectors
joints.cvec <- list(c(1,0,0), c(1,0,0), NA, c(0,0,1))

## Define the linkage
linkage <- defineLinkage(joints=joints, joints.cvec=joints.cvec, 
    joints.type=c("R", "U", "U", "R"))

## Animate the linkage by rotating the input link over 0.2 radians
link_anim <- animateLinkage(linkage, t=seq(0, 0.2, length=50))

## Draw the linkage using native R plotting
drawLinkage(link_anim, method="plot")

}