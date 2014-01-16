# input: latlon obj, station to eliminate
# output: same obj, with station eliminated

# function: remove a station from the latlon object
#           without having to recalculate the whole thing.
update.latlon = function(obj,station){


    
    # eliminate station from mat
    
#    ! Input: distMat - lookup distance matrix, double precision (dn x 3)
#!        dn - row count of distMat, integer
#!        stID - station id to remove, integer
#!        returnMat - return matrix w/o stID, double precison (rn x 3)
#!        rn - row count of returnMat, integer
#!             rn =  .5 * (dn^2 - dn) # sum (i..(dn-1))


    obj$mat = subset(obj$mat,obj$mat[,1] != station & obj$mat[,2] != station)

    # end change
    if(length(obj$mat) < 9){
      
      obj$n = 1
      obj$st1 = obj$mat[1]
      obj$st2 = obj$mat[2]
      obj$ps = obj$ps[obj$ps[,1] != station,]
      obj$freq = 1
      
      return(obj)
    }
    # eliminate station from ps
    obj$ps = obj$ps[obj$ps[,1] != station,]
    # find new min dist
  
    obj$old.min.dist = obj$min.dist # save off old min.dist for boundary checking.
    obj$min.dist = min(obj$mat[,3])
    # how many times does this min dist occur
    stations = grep(obj$min.dist,obj$mat[,3])
    obj$freq = length(grep(obj$min.dist,obj$mat[,3]))
    # what is the first pair of stations at which this occurs?
    obj$st1 = obj$mat[stations[1],1]
    obj$st2 = obj$mat[stations[1],2]
    obj$n = nrow(obj$ps)
  
  

  return(obj)


}