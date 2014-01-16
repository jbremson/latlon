# A wrapper function for update.f

# 7/3/2006 1:47:23 PM

#! Input: distMat - lookup distance matrix, double precision (dn x 3)
#!        dn - row count of distMat, integer
#!        stID - station id to remove, integer
#!        returnMat - return matrix w/o stID, double precison (rn x 3)
#!        rn - row count of returnMat, integer
#!             rn =  .5 * (dn^2 - dn) # sum (i..(dn-1))
#

update.fortran = function(obj,stID){


  ret.n = .5 * ((obj$n-1)^2 - (obj$n -1))

  if(ret.n > 0){
    ret = matrix(-1,ret.n,3)
    nrows = nrow(obj$mat)

    o = .Fortran("update",as.matrix(obj$mat),
                        as.integer(nrows),
                        as.integer(stID),
                        as.matrix(ret),
                        as.integer(ret.n))
                        
  } else {
    stop("Return matrix has row count of less than 1")
  }

  return(o[[4]])
}



