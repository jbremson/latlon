# found.boundary
#
# input:   min.dist      - The current mininum distance in the distance matrix.
#          old.min.dist  - The prior minimum distance in the distance matrix,
#                          prior to the last update.
#          verbose       - optional. Print stuff to screen when run.
#          boundaries    - optional. A list of boundaries to use.
#
# output: out$bool       - boolean. Did these two vars cross a boundary of
#                          interest?
#         out$boundary   - real. The boundary point value that was crossed.
#         out$count      - integer. How many boundaries were crossed. Hopefully
#                          either 1 or 0 !!!
#
# function:    Given a list of boundaries values such as {1,2,3,5}, this
#              function checks to see whether two scalar input values span
#              one of the boundary points. For example the inputs (1.9,2.1)
#              span the boundary point '2'.
#
#              The code will default to a preset boundary list. This can
#              be overriden by setting the boundaries argument (list).
#
# algorithm:   This function takes advantage of the fact that the product
#              of the inputs when subtracted from the a boundary will be
#              negative, and under non-boundary conditions it will not.
#
#              If a negative value is returned, and there should only be 1
#              if any given a non-degenerate set, then we check the index
#              of the negative value and use it to look up the boundary point.
#
              
found.boundary = function(min.dist,old.min.dist,verbose=FALSE,
                  boundaries=c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1,1.333,1.667,2,3,4,5))
{
  out = list()
  x = (boundaries - min.dist) * (boundaries - old.min.dist)

  out$x = x
  # if x contains a negative or 0 val then a boundary has been crossed.
  negs = x[x <= 0]
  loc = length(negs)
  if(loc > 0){
    index = grep(negs[loc],x) # get only the last value
    # if there is more than one occurence of negs[loc] in x
    # then we have a problem to deal with.
    index = max(index) # solved - just take the last index
  }
  if( loc > 1){
    warning("Multiple boundaries were crossed at the distance pair:")
    cat(min.dist, " ", old.min.dist,"\n")
    cat("Using larger boundary value.\n")
    out$bool = TRUE
    out$boundary = boundaries[index]
    out$count = loc
  }

  if (loc == 1){
    # we've found a normal boundary pair
    out$bool = TRUE
    out$boundary = boundaries[index]
    out$count = 1
  }
  
  if (loc == 0){
    out$bool = FALSE
    out$boundary = NA
    out$count = 0
  }

  if(verbose & out$bool == TRUE){
    cat("Verbose mode for found.boundary\n")
    cat("Boundaries are: ", boundaries, "\n")
    print(out)
  }
  
  return(out)


}
              