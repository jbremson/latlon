\name{dist.latlon}
\alias{dist.latlon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ A function to get a distance matrix from a points matrix in longitude latitude}
\description{
     Take a n x 3 points matrix and convert it to a m x 3 distance matrix. 
     
     First column of points matrix is the integer id of the
     long-lat pair
     
     Second column is the latitude.
     
     Third column is the longitude.

}
\usage{
out = dist.latlon(x)
}

\value{
  Returns a m x 3 ( m = sum(seq(1,n-1)) ) lookup distance matrix.
  
  Using a rectangular matrix allows an easy lookup of the point pair(s) 
  corresponding to a distance.
  
  First column is the id of the first station in the distance pair.
  
  Second column is the id of the second station in the distance pair.
  
  Third column is the distance, in miles, between the id pair.

}

\author{ Joel Bremson - jbremson@ucdavis.edu}

\seealso{ dist }

\examples{
data(birmingham)
o = dist.latlon(birmingham)

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as

dist.latlon=function(ps){

  n = nrow(ps)
  sum.n = sum(seq(1,n-1))

  output = matrix(-1,sum.n,3)

  o = .Fortran("latlon",as.matrix(ps),as.integer(n),as.matrix(output),
        as.integer(sum.n))
  return(o[[3]])



}

}
\keyword{ dist }% at least one, from doc/KEYWORDS

