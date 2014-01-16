# describe.vdm

# function: Make a numeric description of the vol.dist.mat upper
#           and lower deciles (or other sized region).

# input: vdm obj, lower region cutoff (optional), upper region cutoff (optional
#         optional regions default to deciles

# output: lower decile average and variance, upper decile average and
#         variance, we could also give information on the percentage
#         of stations within 3 sd of the decile mean.


describe.vdm = function(obj,upper = .90,lower = .10){
   
   # Add error check - if upper and lower not in (0,1)
   #     then do error...
   vdm = obj$vol.dist.mat
   # use first volume column - should be original values?
   vol.col = vdm[,3]
   u = quantile(vol.col,upper)
   l = quantile(vol.col,lower)
   
   u.vol = vol.col[vol.col >= u]
   l.vol = vol.col[vol.col <= l]
   cat("===================================================\n")
   cat("City Name: ", obj$city.name,"\n")
   cat("Total Time Cost =",obj$total.time.cost,"\n\n") 
   cat("Upper ", upper*100, "% Description\n===============================\n")
   print(summary(u.vol))
   cat("(quartiles of top decile original volumes)\n")
   cat("Standard Deviation: " ,sd(u.vol), "\n")
   u.n = round((1 - upper) * length(vol.col))
   cat("# stations: ", u.n, " of " , length(vol.col), "\n")
   cat("\n")
   cat("Lower ", lower*100 , "% Description\n===============================\n")
   print(summary(l.vol))
    cat("(quartiles of bottom decile original volumes)\n")
   cat("Standard Deviation: " ,sd(l.vol), "\n")
   l.n = round(lower * length(vol.col))
   cat("# stations: ", l.n, " of " , length(vol.col), "\n\n")
   

   

}
   

# Set the S3 generic
describe = function(obj){ UseMethod("describe",obj)}

write = function(obj){UseMethod("write",obj)}

write.vdm = function(obj,name=file.choose()){

  v = obj$vol.dist.mat
  write.table(v,file=name,row.names=FALSE)
}






