#  set.names
#
#  function: Set up the names of the vol.dist.mat so it each column
#            can include the cluster distance. For example with a cluster
#            distance of 0.5 the names returned would be:
#
#            lat-0.5 long-0.5 vol-0.5
#
#  input: cluster distance (real)
#
#  output: list, length = 3, type=string

set.names = function(dist){
  o = c()
  o[1] = paste("lat-",dist,sep='')
  o[2] = paste("long-",dist,sep='')
  o[3] = paste("vol-",dist,sep='')
  return(o)
}